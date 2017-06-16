#ifndef _P2PACK_H
#define _P2PACK

#include <string>
#include <iostream>
#include <cstdint>
#include <fstream>
#include <string.h>
#include <list>
#include <stdlib.h>
#include <sstream>
#include <string>


// Exception codes
#define EX_MEM_ALLOC_FAIL 0x0001;


using namespace std;

// File format constants
// The block size in bytes
// Set to 2MB because this allows accessing the full file size
// Allowable by javascript MAX_SAFE_INTEGER to address file positions
// Number.MAX_SAFE_INTEGER = 9007199254740991
// Max Int32 Value: 4294967295
// Number.MAX_SAFE_INTEGER / 4294967295 = 2097152 =  2 * (1024)^2
#define FILE_BLOCK_SIZE ((uint64_t) 2097152)

// File format stucts
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


struct indexEntry {
  char key[128];
  uint32_t sizeBlocks; // size in blocks
  uint32_t offset; // In blocks after index
  uint32_t flags;
};


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
// Program structs
// Keeping track of entries

struct entry {
  char key[128];
  void* payload;
  uint64_t size; // bytes
  uint32_t blockSize; // size in blocks as will be written in the index
};


// Function prototypes
void print_file_format_info();
void make_file(string &indir, string &outfile);
string readWholeFile(string &filename);
void make_file_from_payload(list<entry> &entries, string &filename);
void makeMainSparseMatrix(list<entry> &entries, string &indir);

void makeAspectSparseMatrix(list<entry> &entries, string &indir);

template<class T>
list<T>* readNumberArrayFromFile(string &filename);

template <typename T> inline T intDivRoundUP(T a, T b) {
return(a + b -1) /b;
}

#endif
