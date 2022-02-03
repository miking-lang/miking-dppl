#include <string>
#include <fstream>
#include <sstream>
#include <list>
#include <iterator>

#include "file_handler.cuh"


void prepareFile(std::string fileName, bool truncate) {
    std::ofstream resFile (fileName, (truncate ? std::ofstream::out | std::ofstream::trunc : std::ios_base::app));
    resFile << "[";
    resFile.close();
}

void finishFile(std::string fileName, bool removeLast) {
    std::ifstream resFile1 (fileName);
    std::stringstream buffer;
    buffer << resFile1.rdbuf();
    std::string contents = buffer.str();
    resFile1.close();
    if(removeLast)
        contents.pop_back();
    contents += "]\n";
    std::ofstream resFile2 (fileName,  std::ofstream::out | std::ofstream::trunc);
    resFile2 << contents;
    resFile2.close();
}

void writeLogNormConstToFile(double logNormConstant) {
    // std::ofstream resFile (fileName);
    std::ofstream resFile (Z_FILE_NAME, std::ios_base::app); // If append to file is wanted
    if(resFile.is_open()) {

        resFile << logNormConstant << ",";
        resFile.close();
    } else {
        printf("Could not open file %s\n", Z_FILE_NAME.c_str());
    }
}


void writeESSToFile(std::list<double> essList) {
    // std::ofstream resFile (fileName);
    std::ofstream resFile (ESS_FILE_NAME, std::ios_base::app); // If append to file is wanted
    if(resFile.is_open()) {
        resFile << "[";
        std::list <double> :: iterator it;
        for(it = essList.begin(); it != essList.end(); ++it)
            resFile << *it << " ";
        resFile << "],\n";
        resFile.close();
    } else {
        printf("Could not open file %s\n", ESS_FILE_NAME.c_str());
    }
}
