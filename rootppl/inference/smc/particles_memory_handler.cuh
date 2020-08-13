#ifndef PARICLES_MEMORY_HANDLER_INCLUDED
#define PARICLES_MEMORY_HANDLER_INCLUDED

/*
 * File particles_memory_handler.cuh contains helper functions used by the smc implementation files. 
 * Functions for allocating and deleting particles belongs here. The memory management differs in 
 * top-level SMC and nested SMC, since the top-level will differ depending whether its compiled for CPU 
 * or GPU. While nested inference is done either on the device or CPU and both have 
 * the same API (as opposed to the host functions for handling memory on the device). 
 */

/**
 * Allocates particles for top-level SMC.
 * 
 * @param numParticles the number of particles that should be allocated.
 * @param progStateSize the size of the particles program states in bytes. 
 * @param printMemSize whether the total size of the allocated particles should be printed. 
 * @return particle structure with pointers to the allocated data.
 */
particles_t allocateParticles(int numParticles, size_t progStateSize, bool printMemSize=false);

/**
 * Frees particles for top-level SMC.
 *  
 * @param particles structure with pointers to the allocated data.
 */
void freeParticles(particles_t particles);

/**
 * Allocates particles for nested SMC.
 *  
 * @param numParticles the number of particles that should be allocated.
 * @param progStateSize the size of the particles program states in bytes. 
 */
HOST DEV particles_t allocateParticlesNested(int numParticles, size_t progStateSize);

/**
 * Frees particles for nested SMC.
 *  
 * @param particles structure with pointers to the allocated data.
 */
HOST DEV void freeParticlesNested(particles_t particles);

#endif