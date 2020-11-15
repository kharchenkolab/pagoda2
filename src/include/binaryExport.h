#ifndef __BINARYEXPORT__
#define __BINARYEXPORT__

#include <string>
#include <iostream>
#include <cstdint>
#include <fstream>
#include <string.h>
#include <list>
#include <stdlib.h>
#include <sstream>
#include <string>

#include "pagoda2.h"

// Exception codes
#define EX_MEM_ALLOC_FAIL 0x0001


using namespace std;

// File format constants
// The block size in bytes
// Set to 2MB because this allows accessing the full file size
// Allowable by javascript MAX_SAFE_INTEGER to address file positions
// Number.MAX_SAFE_INTEGER = 9007199254740991
// Max Int32 Value: 4294967295
// Number.MAX_SAFE_INTEGER / 4294967295 = 2097152 =  2 * (1024)^2
//#define FILE_BLOCK_SIZE ((uint64_t) 2097152) // This allows max file size allowable by integer but produces larger files
#define FILE_BLOCK_SIZE ((uint64_t) 32768) // 32k block size


////////////////////////////////////
// File format structs
////////////////////////////////////

// Main file header
struct fileHeader {
  char identifier[32];
  uint8_t versionMajor;
  uint8_t versionMinor;
  uint16_t flags;
  // Uint32 because JS doesn't support 64 bit ints
  uint32_t blockSize; // In bytes
  uint32_t headerSize; // In bytes
  uint32_t indexSize; // In bytes
};

// Index entry
struct indexEntry {
  char key[128];
  uint32_t sizeBlocks; // size in blocks
  uint32_t offset; // In blocks after index
  uint32_t flags;
};

// Header for sparse matrix
struct sparseMatrixHeader {
  // Offsets with respect to the beginning of the beginning of the
  // First block
  uint32_t dim1;
  uint32_t dim2;
  uint32_t pStartOffset;
  uint32_t iStartOffset;
  uint32_t xStartOffset;
  uint32_t dimname1StartOffset;
  uint32_t dimname2StartOffset;
  uint32_t dimname2EndOffset;
};

// Program structs

// For keeping track of entries before writing the to disk
struct entry {
  char key[128];
  void* payload;
  uint64_t size; // bytes
  uint32_t blockSize; // size in blocks as will be written in the index
};

struct binaryExportParams {
  bool verbose;
  bool verboseTimings;
};

///////////////////////////////
// Function Prototypes
///////////////////////////////
template <class T>
std::list<T>* IVtoL(Rcpp::IntegerVector f);

template <class T>
  std::list<T>* NVtoL(Rcpp::NumericVector f);

template <typename T> inline T intDivRoundUP(T a, T b) {
  return(a + b -1) /b;
}

struct entry *make_entry_from_string(char const *key, string &data);


void addSparseMatrixToEntries(List spML,
                              list<entry> &entries,
                              char const *mattype,
                              binaryExportParams *params);

#endif
