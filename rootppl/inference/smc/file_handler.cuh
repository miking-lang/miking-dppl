#ifndef FILE_HANDLER_INCLUDED
#define FILE_HANDLER_INCLUDED

#include <string>
#include <list>

const std::string Z_FILE_NAME = "log_norm_const.txt";
const std::string ESS_FILE_NAME = "ess.txt";


/**
 * Optionally clears the file, and writes an opening bracket to it.
 *
 * @param fileName the name of the file to save to.
 * @param truncate if the file should be cleared first.
 */
void prepareFile(std::string fileName, bool truncate);


/**
 * Optionally removes the last character of the file, and writes a closing bracket and new line to it.
 *
 * @param fileName the name of the file to save to.
 * @param removeLast if the last character of the file should be deleted before closing it.
 */
void finishFile(std::string fileName, bool removeLast);

/**
 * Writes the approximated log normalization constant to file. The file is cleared before the
 * program starts and if multiple runs is performed, the values from each run is appended to this file.
 *
 * @param logNormConstant
 */
void writeLogNormConstToFile(double logNormConstant);

/**
 * Writes the effective sample size (ESS) to file. The file is cleared before the program
 * starts.
 *
 * @param ess
 */
void writeESSToFile(std::list<double> essList);

#endif