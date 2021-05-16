
# ifndef model
# $(error model argument needs to be set with e.g.: $(\n)make model=path/to/model.cu$(\n))
# endif

ifndef EXEC_NAME
EXEC_NAME=program
endif

ifdef arch
# GPU
# CC=/usr/local/cuda-11.0/bin/nvcc
CC=nvcc
FLAGS=-I. -std=c++14 -arch=sm_$(arch) -rdc=true -lcudadevrt -O3
FLAGS_LINK=-std=c++14 -arch=sm_$(arch) -rdc=true -lcudadevrt -O3
else
# CPU
CC=$(CXX)
FLAGS=-I. -xc++ -std=c++14 -O3 $(OMP)
FLAGS_LINK=-std=c++14 -O3 $(OMP)
endif

# EXEC_NAME=program

.PHONY :\
	install
	clean
	framework
	ompframework
	omp
	openmpflag

SMC_SRC=inference/smc/smc.cu
SMC_NESTED_SRC=inference/smc/smc_nested.cu
DISTS_SRC=dists/dists.cu
SCORES_SRC=dists/scores.cu
PARTICLES_MEMORY_HANDLER_SRC=inference/smc/particles_memory_handler.cu
MISC_SRC=utils/misc.cu
MATH_SRC=utils/math.cu
SMC_KERNELS_SRC=inference/smc/smc_kernels.cu
SYSTEMATIC_COMMON_SRC=inference/smc/resample/common.cu
SYSTEMATIC_SEQ_SRC=inference/smc/resample/systematic/systematic_cpu.cu
SYSTEMATIC_PARALLEL_SRC=inference/smc/resample/systematic/systematic_gpu.cu
SYSTEMATIC_KERNELS_SRC=inference/smc/resample/systematic/kernels.cu
FILE_HANDLER_SRC=inference/smc/file_handler.cu

OBJDIR=out
_OBJ_FILES_FRAMEWORK=smc.o smc_nested.o dists.o scores.o particles_memory_handler.o misc.o math.o \
	smc_kernels.o systematic_common.o systematic_seq.o systematic_parallel.o systematic_kernels.o file_handler.o
OBJ_FILES_FRAMEWORK=$(patsubst %.o, $(OBJDIR)/%.o, $(_OBJ_FILES_FRAMEWORK))

# SMC_DEPS = $(wildcard inference/smc/*.cuh) $(wildcard inference/smc/resample/*/*.cuh) $(wildcard macros/*.cuh) dists/dists.cuh utils/cuda_error_utils.cuh utils/misc.cuh utils/math.cuh
FRAMEWORK_FILES = $(wildcard inference/*) $(wildcard inference/*/*) $(wildcard inference/*/*/*) $(wildcard inference/*/*/*/*) \
	$(wildcard dists/*) $(wildcard macros/*) $(wildcard utils/*)

# Link object files to executable
$(EXEC_NAME): $(OBJDIR) $(OBJDIR)/model.o $(OBJ_FILES_FRAMEWORK)
	$(CC) $(FLAGS_LINK) out/*.o -o $@
	rm $(OBJDIR)/model.o

bin_path = $(HOME)/.local/bin
rootppl_name = rootppl
install:
	mkdir -p $(bin_path)
	cp -f ./$(rootppl_name) $(bin_path)/$(rootppl_name)
	chmod +x $(bin_path)/$(rootppl_name)
	

clean:
	rm -f out/*.o

run:
	./$(EXEC_NAME) $(N)

openmpflag:
	$(eval OMP = -fopenmp)

# Compile only framework
framework:\
	$(OBJDIR) $(OBJ_FILES_FRAMEWORK)

ompframework:\
	openmpflag $(OBJDIR) $(OBJ_FILES_FRAMEWORK)

omp:\
	openmpflag $(EXEC_NAME)


# Compile model, which is always recompiled as it is deleted after the executable is created.
$(OBJDIR)/model.o: $(model)
	$(CC) -c $(FLAGS) $(model) -o $@
	
# $(CC) -c $(FLAGS) $(model) -o model.o

$(OBJDIR):
	mkdir $(OBJDIR)


# $@ is target name
# $< is dependency list

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

$(OBJDIR)/file_handler.o: $(FRAMEWORK_FILES)
	$(CC) -c $(FLAGS) $(FILE_HANDLER_SRC) -o $@
