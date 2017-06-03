#ifndef _PAGODA2PACKER_H
#define _PAGODA2PACKER

#include <string>
#include <iostream>
#include <cstdint>
#include <fstream>
#include <string.h>
#include <list>
#include <stdlib.h>
#include <sstream>
#include <string>

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

// Program structs
// Program structs
// Keeping track of entries

struct entry {
  char key[128];
  void* payload;
  uint64_t size; // bytes
  uint32_t blockSize; // size in blocks as will be written in the index
};

// DEBUG or DEMO defines
#define MANY_AS "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
#define MANY_BS "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"



// Function prototypes
void print_file_format_info();
void make_dummy_file();
void make_file(string &indir, string &outfile);
string readWholeFile(string filename);
void make_file_from_payload(list<entry> &entries, string &filename);

template <typename T> inline T intDivRoundUP(T a, T b) {
  return(a + b -1) /b;
}

#endif
