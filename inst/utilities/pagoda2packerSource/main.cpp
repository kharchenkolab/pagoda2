/**
 * Author: Nikolas Barkas
 * Date:  June 2017
 * Description: pagoda2 packer, packing program for pagoda2
 */
#include "header.h"

using namespace std;


int main( int argc, char *argv[] ) {
  string indir = "/home/barkasn/testSerial/";
  string outfile = "./output.p2s";

  make_file(indir, outfile);

  return 0;
  
}

void make_dummy_file() {
  print_file_format_info();

  ///////////////////////////////////////////////////////////
  // HEADER
  //////////////////////////////////////////////////////////
  
  // Generate a header
  struct fileHeader header;

  // Clear the memory to avoid confusion
  memset(&header, 0, sizeof(header));
  
  strcpy(header.identifier, "pagoda2datafile");
  header.versionMajor = 1;
  header.versionMinor = 0;
  header.flags = 0xFFFF;

  header.blockSize = FILE_BLOCK_SIZE;
  header.headerSize = sizeof(struct fileHeader);

  // We will need to come back to this later to update tin the index size

  //////////////////////////////////////////////////////////
  // Generating dummy entries
  //////////////////////////////////////////////////////////

  
  list<entry> entries;
  
  struct entry e1;
  memset(&e1, 0, sizeof(entry));
  strcpy(e1.key, "entry1");
  e1.payload = malloc(100);
  memcpy(e1.payload, MANY_AS, 100);
  e1.size = 100; // bytes
  e1.blockSize = 1;
  entries.push_back(e1);
  
  struct entry e2;
  memset(&e2, 0, sizeof(entry));
  strcpy(e2.key, "entry2");
  e2.payload = malloc(20);
  memcpy(e2.payload, MANY_BS, 20);
  e2.size = 20; // byes
  e2.blockSize = 2;
  entries.push_back(e2);

  struct entry e3;
  memset(&e3, 0, sizeof(entry));
  strcpy(e3.key, "entry3");
  e3.payload = malloc(20);
  memcpy(e3.payload, MANY_BS, 20);
  e3.size = 20; // byes
  e3.blockSize = 1;
  entries.push_back(e3);


  //////////////////////////////////////////////////////////
  // Updata index size
  //////////////////////////////////////////////////////////

  // Size of index in bytes -- update in header
  uint64_t indexSize = sizeof(indexEntry) * entries.size();
  header.indexSize = indexSize;
  cout << "The index size is: " << indexSize << endl;

  // Construct the index in memory
  list<indexEntry> indexEntries;
  uint32_t curOffset = 0; // in blocks
  for(list<entry>::iterator iterator = entries.begin(); iterator != entries.end(); ++iterator) {
    struct indexEntry ie;
    
    memcpy(ie.key, iterator->key, 128);

    ie.sizeBlocks = iterator-> blockSize;
      
    // Update the offset
    ie.offset = curOffset;

    // Flags are reserved for future use
    ie.flags = 0;

    // Put on the output list
    indexEntries.push_back(ie);

    // Increment the offset
    curOffset += iterator->blockSize;
  }


  // Open the file
  ofstream fs;
  fs.open("p2demo.bin", ios::out | ios::binary);

  // Write the header
  fs.write((const char*) &header, sizeof(header));

  // Write the index entries
  for (list<indexEntry>::iterator iterator = indexEntries.begin(); iterator != indexEntries.end(); ++iterator) {
    fs.write((const char*) &(*iterator), sizeof(indexEntry));
  }

  // Write the content
  int i = 0;
  for(list<entry>::iterator iterator = entries.begin(); iterator != entries.end(); ++iterator) {
    cout << "Writing entry " << i++ << " ..." << endl;
    size_t s = iterator-> blockSize * FILE_BLOCK_SIZE;

    cout << "Entry size is " <<  iterator->blockSize << " blocks or " << s << " bytes" << endl;
    void *entry = malloc(s); // memspace for entry as will be on disk
    // TODO: Check entry != 0

    memset(entry,  0, s); // fill with 0s
    memcpy(entry, (const char*) iterator->payload, iterator->size); // copy the payload
    
    // Write to file
    fs.write( (const char*) entry, s);

    free(entry);
  }

  // Close the file
  fs.close();

}


void print_file_format_info() {
  cout << "Size of indexEntry: " << sizeof(indexEntry) << endl;
  cout << "Size of fileHeader: " << sizeof(fileHeader) << endl;
}

struct entry* make_entry_from_string(char const *key, string &data) {
  struct entry *e;
  e = (struct entry*) malloc(sizeof(struct entry));
  //malloc
  
  memset(e, 0, sizeof(entry));
  strcpy(e-> key, key);
  uint64_t entryLengthBytes = data.length();
  e->payload = malloc(entryLengthBytes); // second allo, in case we want to free this need to be freed too
  memcpy(e->payload, data.c_str(), entryLengthBytes);
  e->size = entryLengthBytes;
  e->blockSize = (uint32_t) intDivRoundUP(entryLengthBytes, FILE_BLOCK_SIZE);

  return e;
}

/**
 * Pack the contents of the specified indir
 * into the outfile. The indir must contain files
 * exported by the pagoga2 disk serializer
 */
void make_file(string &indir, string &outfile) {
  // Generate file names
  string cellmetadataFile = indir + "cellmetadata.json";
  string cellorderFile = indir + "cellorder.json";
  string geneinformationFile = indir + "geneinformation.json";
  string reduceddendrogramFile = indir + "reduceddendrogram.json";
  string embeddingstructureFile = indir + "embeddingstructure.json";

  string cellmetadataData = readWholeFile(cellmetadataFile);
  string cellorderData = readWholeFile(cellorderFile);
  string geneinformationData = readWholeFile(geneinformationFile);
  string reduceddendrogramData = readWholeFile(reduceddendrogramFile);
  string embeddingstructureData = readWholeFile(embeddingstructureFile);


  list<entry> entries;
  
  // struct entry is for the program only
  /*
  struct entry e;
  memset(&e, 0, sizeof(entry)); // clear mem
  strcpy(e.key, "cellmetadata");
  uint64_t entryLengthBytes = cellmetadataData.length();
  e.payload = malloc(entryLengthBytes);
  memcpy(e.payload, cellmetadataData.c_str(), entryLengthBytes);
  e.size = entryLengthBytes; // bytes
  e.blockSize = (uint32_t) intDivRoundUP(e.size, FILE_BLOCK_SIZE);
  */

  // Cast required to avoid warning
  struct entry* metadataEntry =  make_entry_from_string("cellmetadata", cellmetadataData);
  entries.push_back(*metadataEntry);

  struct entry* cellorderEntry = make_entry_from_string("cellorder", cellorderData);
  entries.push_back(*cellorderEntry);

  struct entry* geneinformationEntry = make_entry_from_string("geneinformation", geneinformationData);
  entries.push_back(*geneinformationEntry);

  struct entry* reduceddendrogramEntry = make_entry_from_string("reduceddendrogram", reduceddendrogramData);
  entries.push_back(*reduceddendrogramEntry);

  struct entry* embeddingstructureEntry = make_entry_from_string("embeddingstructure", embeddingstructureData);
  entries.push_back(*embeddingstructureEntry);
  // CONTINUE HERE WITH GENERATION OF OTHER ENTRIES


  // Doing the embeddings here  -- this needs to be dynamic of a list of files staring with emb*
  // Alternatively it could be a command line argument
  string embPCAlargeVisFile = indir + "emb_PCA_largeVis.json";
  string embPCAlargeVisData = readWholeFile(embPCAlargeVisFile);
  struct entry* embPCAlargeVisEntry = make_entry_from_string("emb_PCA_largeVis", embPCAlargeVisData);
  entries.push_back(*embPCAlargeVisEntry);
  
  string embPCAtSNEFile = indir + "emb_PCA_tSNE.json";
  string embPCAtSNEData = readWholeFile(embPCAtSNEFile);
  struct entry* embPCAtSNEEntry = make_entry_from_string("emb_PCA_tSNE", embPCAtSNEData);
  entries.push_back(*embPCAtSNEEntry);




  
  // Make the main sparse matrix
  // Read the p
  string matsparsePFile = indir + "matsparse_p.txt";
  list<uint32_t> *pData;
  pData = readNumberArrayFromFile<uint32_t>(matsparsePFile);
  cout << "p array size: " << pData->size() << endl;
  cout << "p first entry: " << pData->front() << endl;

  // Read the i
  string matsparseIFile = indir + "matsparse_i.txt";
  list<uint32_t> *iData;
  iData = readNumberArrayFromFile<uint32_t>(matsparseIFile);
  cout << "i array size: " << iData->size() << endl;
  cout << "i first entry: " << iData->front() << endl;

  // Read the x
  string matsparseXFile = indir + "matsparse_x.txt";
  list<float> *xData;
  xData = readNumberArrayFromFile<float>(matsparseXFile);
  cout << "x array size: " << xData->size() << endl;
  cout << "x first entry: " << xData->front() << endl;

  // Read dim names 1
  string matsparseDimnames1File = indir + "matsparse_Dimnames1.json";
  string matsparseDimnames1 = readWholeFile(matsparseDimnames1File);

  // Read dimnames 2
  string matsparseDimnames2File = indir + "matsparse_Dimnames2.json";
  string matsparseDimnames2 = readWholeFile(matsparseDimnames2File);

  string dimFile = indir + "matsparse_Dim.txt";
  list<uint32_t> *Dim;
  Dim = readNumberArrayFromFile<uint32_t>(dimFile);
   
  // Now serialise
  struct sparseMatrixHeader smh;
  list<uint32_t>::iterator li = Dim->begin();
  smh.dim1 = *li;
  li++;
  smh.dim2 = *li;

  // Always 2
  smh.pStartOffset = sizeof(struct sparseMatrixHeader);
  smh.iStartOffset = smh.pStartOffset + sizeof(uint32_t) * pData->size();
  smh.xStartOffset = smh.iStartOffset + sizeof(uint32_t) * iData->size();
  smh.dimname1StartOffset = smh.xStartOffset + sizeof(uint32_t) * xData->size();
  smh.dimname2StartOffset = smh.dimname1StartOffset + matsparseDimnames1.size();
  smh.dimname2EndOffset = smh.dimname2StartOffset + matsparseDimnames2.size();
  

  cout << "matsparse header information" << endl;
  cout << "dim1 " << smh.dim1 << endl;
  cout << "dim2 " << smh.dim2 << endl;
  cout << "pStartOffset " << smh.pStartOffset << endl;
  cout << "iStartOffset " << smh.iStartOffset << endl;
  cout << "xStartOffset " << smh.xStartOffset << endl;
  cout << "dimnames1StartOffset " << smh.dimname1StartOffset << endl;
  cout << "dimnames2StartOffset " << smh.dimname2StartOffset << endl;
  cout << "dimnames2EndOffset " << smh.dimname2EndOffset << endl;

  cout << endl << "dimnames1 size" <<  matsparseDimnames1.size() << endl;

  //cout << "Test " << matsparseDimnames1 << endl;
  // Make a memory holder for the data
  stringstream smhData(stringstream::in|stringstream::out|stringstream::binary);
  // Write the header
  smhData.write((const char*) &smh, sizeof(smh));
  //cout << "Size of sparse header " << sizeof(smh) << endl;
  
  
  // Write the p object
  for(list<uint32_t>::const_iterator iter = pData->begin(); iter != pData->end(); ++iter) {
    smhData.write((const char*) &*iter, sizeof(uint32_t));
  }

  // Write the i object
  for(list<uint32_t>::const_iterator iter = iData->begin(); iter != iData->end(); ++iter) {
    smhData.write((const char*) &*iter, sizeof(uint32_t));
  }

  // Write the x object
  for(list<float>::const_iterator iter = xData->begin(); iter != xData->end(); ++iter) {
    //smhData << *iter;
    smhData.write((const char*) &*iter, sizeof(uint32_t));
  }

  // Write the Dimnames as JSON string
  // HERE
  smhData.write( matsparseDimnames1.c_str(), matsparseDimnames1.size());
  smhData.write( matsparseDimnames2.c_str(), matsparseDimnames2.size());

  //cout << matsparseDimnames1.c_str();
  
  // Convert the buffer to a string
  string smhDataString = smhData.str();


  struct entry* sparseMatrixEntry = make_entry_from_string("sparseMatrix", smhDataString);
  entries.push_back(*sparseMatrixEntry);
  
  //smhData << smh;
  
  make_file_from_payload(entries, outfile);

}

template<class T>
list<T>* readNumberArrayFromFile(string &filename) {
  list<T>* pData;
  pData = new list<T>;
  
  string matsparsePFile = filename;
  
  //cout << "Sparse matrix file: " << matsparsePFile << endl;
  
  ifstream fs;
  fs.open(matsparsePFile, ios::in | ios::binary);
  
  T number;
  while(fs >> number) {
    pData->push_back(number);
  }
  
  //cout << "pData size is :" << pData.size() << endl;
  fs.close();

  return pData;
}


string readWholeFile(string &filename) {
  ifstream in;
  in.open(filename, ifstream::in);
  // https://stackoverflow.com/questions/116038/what-is-the-best-way-to-read-an-entire-file-into-a-stdstring-in-c
  string data = static_cast<stringstream const&>(stringstream() << in.rdbuf()).str();
  in.close();

  return data;
}



void make_file_from_payload(list<entry> &entries, string &filename) {
  print_file_format_info();

  // Open the file
  ofstream fs;
  fs.open(filename, ios::out | ios::binary);

  // Generate a header
  struct fileHeader header;

  // Clear the memory to avoid confusion
  memset(&header, 0, sizeof(header));
  
  strcpy(header.identifier, "pagoda2datafile");
  header.versionMajor = 1;
  header.versionMinor = 0;
  header.flags = 0xFFFF;

  header.blockSize = FILE_BLOCK_SIZE;
  header.headerSize = sizeof(struct fileHeader);
  header.indexSize = sizeof(indexEntry) * entries.size();

  // TODO if verbose
  cout << "The index size is: " << header.indexSize << endl;

  // Construct the index in memory
  list<indexEntry> indexEntries;
  uint32_t curOffset = 0; // in blocks
  for(list<entry>::iterator iterator = entries.begin(); iterator != entries.end(); ++iterator) {
    struct indexEntry ie;

    // Copy the key
    memcpy(ie.key, iterator->key, 128);

    ie.sizeBlocks = iterator-> blockSize;
      
    // Update the offset
    ie.offset = curOffset;

    // Flags are reserved for future use
    ie.flags = 0;

    // Put on the output list
    indexEntries.push_back(ie);

    // Increment the offset
    curOffset += iterator->blockSize;
  }

  // Write the header
  fs.write((const char*) &header, sizeof(header));

  // Write the index entries
  for (list<indexEntry>::iterator iterator = indexEntries.begin(); iterator != indexEntries.end(); ++iterator) {
    fs.write((const char*) &(*iterator), sizeof(indexEntry));
  }

  // Write the content
  int i = 0;
  for(list<entry>::iterator iterator = entries.begin(); iterator != entries.end(); ++iterator) {
    cout << "Writing entry " << i++ << " ..." << endl;
    size_t s = iterator-> blockSize * FILE_BLOCK_SIZE;

    cout << "Entry size is " <<  iterator->blockSize << " blocks or " << s << " bytes" << endl;
    void *entry = malloc(s); // memspace for entry as will be on disk
    // TODO: Check entry != 0

    memset(entry,  0, s); // fill with 0s
    memcpy(entry, (const char*) iterator->payload, iterator->size); // copy the payload
    
    // Write to file
    fs.write( (const char*) entry, s);

    free(entry);
  }

  fs.close();
  
}
