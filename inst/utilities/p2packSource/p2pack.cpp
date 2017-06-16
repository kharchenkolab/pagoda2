/**
 * Author: Nikolas Barkas
 * Date:  June 2017
 * Description: pagoda2 packer, a program that reads exported 
 * pagoda2 object and serialises them into a indexable binary file
 */

#include "p2pack.h"
#include <boost/program_options.hpp>

using namespace std;

int main( int ac, char *av[] ) {

  // Use boose to process command line arguments
  namespace po = boost::program_options;

  po::options_description desc("Allowed options");
  desc.add_options()
    ("help", "produce help message")
    ("output-file", po::value<string>(), "output filename")
    ("input-directory", po::value<string>(), "input directory where object has been serialised (with trailing slash)")
    ;

  po::variables_map vm;
  po::store(po::parse_command_line(ac, av, desc), vm);
  po::notify(vm);

  if (vm.count("help")) {
    cout << desc << endl;
    return 1;
  }

  if (!vm.count("output-file")) {
    cout << "You need to specify an output file" << endl;
    return 0;
  }

  if (!vm.count("input-directory")) {
    cout << "You need to specify an input directory" << endl;
  }

  string indir = vm["input-directory"].as< string >();
  string outfile = vm["output-file"].as< string >();

  try {
    make_file(indir, outfile);
  }
  catch (int e) {
    if (e == EX_MEM_ALLOC_FAIL) {
      cout << "Fatal Exception: Could not allocate required memory" << endl;
      return 1;
    } else {
      cout << "Unknown Exception occured! Exception code: " << e << endl;
      return 1;
    }
  } catch (exception& e) {
    cout << "Standard exception occure: " << e.what() << endl;
    return 1;
  }
  
  return 0;
}

struct entry* make_entry_from_string(char const *key, string &data) {
  struct entry *e;
  e = (struct entry*) malloc(sizeof(struct entry));
  if (e == 0) {
    throw EX_MEM_ALLOC_FAIL;
  }
  
  memset(e, 0, sizeof(entry));
  strcpy(e-> key, key);
  uint64_t entryLengthBytes = data.length();
  e->payload = malloc(entryLengthBytes); // second allo, in case we want to free this need to be freed too
  if (e->payload == 0) {
    throw EX_MEM_ALLOC_FAIL;
  }

  
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
  // List for all the entries
  list<entry> entries;
  
  // Generate file names
  string cellmetadataFile = indir + "cellmetadata.json";
  cout << "Reading " << cellmetadataFile << endl;
  string cellmetadataData = readWholeFile(cellmetadataFile);
  struct entry* metadataEntry =  make_entry_from_string("cellmetadata", cellmetadataData);
  entries.push_back(*metadataEntry);

  string cellorderFile = indir + "cellorder.json";
  cout << "Reading " << cellorderFile << endl;
  string cellorderData = readWholeFile(cellorderFile);
  struct entry* cellorderEntry = make_entry_from_string("cellorder", cellorderData);
  entries.push_back(*cellorderEntry);

  string geneinformationFile = indir + "geneinformation.json";
  cout << "Reading " << geneinformationFile << endl;
  string geneinformationData = readWholeFile(geneinformationFile);
  struct entry* geneinformationEntry = make_entry_from_string("geneinformation", geneinformationData);
  entries.push_back(*geneinformationEntry);
  
  string reduceddendrogramFile = indir + "reduceddendrogram.json";
  cout << "Reading " << reduceddendrogramFile << endl;
  string reduceddendrogramData = readWholeFile(reduceddendrogramFile);
  struct entry* reduceddendrogramEntry = make_entry_from_string("reduceddendrogram", reduceddendrogramData);
  entries.push_back(*reduceddendrogramEntry);

  string embeddingstructureFile = indir + "embeddingstructure.json";
  cout << "Reading " << embeddingstructureFile << endl;
  string embeddingstructureData = readWholeFile(embeddingstructureFile);
  struct entry* embeddingstructureEntry = make_entry_from_string("embeddingstructure", embeddingstructureData);
  entries.push_back(*embeddingstructureEntry);

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

  makeMainSparseMatrix(entries, indir);
  makeAspectSparseMatrix(entries, indir);

  string aspectInformationFile = indir + "aspectInformation.json";
  string aspectInformationData = readWholeFile(aspectInformationFile);
  struct entry* aspectInformationEntry = make_entry_from_string("aspectinformation", aspectInformationData);
  entries.push_back(*aspectInformationEntry);

  string genesetsFile = indir + "genesets.json";
  string genesetsData = readWholeFile(genesetsFile);
  struct entry* genesetsEntry = make_entry_from_string("genesets", genesetsData);
  entries.push_back(*genesetsEntry);

  string genesetsgenesFile = indir + "genesetsgenes.json";
  string genesetsgenesData = readWholeFile(genesetsgenesFile);
  struct entry* genesetsgenesEntry = make_entry_from_string("genesetsgenes", genesetsgenesData);
  entries.push_back(*genesetsgenesEntry);
							       
  
  make_file_from_payload(entries, outfile);
}

void makeMainSparseMatrix(list<entry> &entries, string& indir) {
  cout << "Making expression sparse matrix entry..." << endl;
  
  // Make the main sparse matrix
  // Read the p
  string matsparsePFile = indir + "matsparse_p.txt";
  list<uint32_t> *pData;
  cout << "\tReading " << matsparsePFile << endl;
  pData = readNumberArrayFromFile<uint32_t>(matsparsePFile);
  cout << "\t\tp array size: " << pData->size() << " [First entry value: " << pData->front() << " ]" << endl;
  
  // Read the i
  string matsparseIFile = indir + "matsparse_i.txt";
  list<uint32_t> *iData;
  cout << "\tReading " << matsparseIFile << endl;
  iData = readNumberArrayFromFile<uint32_t>(matsparseIFile);
  cout << "\t\ti array size: " << iData->size() << "[First entry value: " << iData->front() << " ] " << endl;

  // Read the x
  string matsparseXFile = indir + "matsparse_x.txt";
  list<float> *xData;
  cout << "\tReading " << matsparseXFile << endl;
  xData = readNumberArrayFromFile<float>(matsparseXFile);
  cout << "\t\tx array size: " << xData->size() << "[First entry value: " << xData->front() << " ] " << endl;
  
  // Read dim names 1
  string matsparseDimnames1File = indir + "matsparse_Dimnames1.json";
  cout << "\tReading " << matsparseDimnames1File << endl;
  string matsparseDimnames1 = readWholeFile(matsparseDimnames1File);

  // Read dimnames 2
  string matsparseDimnames2File = indir + "matsparse_Dimnames2.json";
  cout << "\tReading " << matsparseDimnames2File << endl;
  string matsparseDimnames2 = readWholeFile(matsparseDimnames2File);

  string dimFile = indir + "matsparse_Dim.txt";
  list<uint32_t> *Dim;
  cout << "\tReading " << dimFile << endl;
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
  

  cout << "\tExpression matrix header information" << endl;
  cout << "\t\tdim1=" << smh.dim1 << endl;
  cout << "\t\tdim2=" << smh.dim2 << endl;
  cout << "\t\tpStartOffset=" << smh.pStartOffset << endl;
  cout << "\t\tiStartOffset=" << smh.iStartOffset << endl;
  cout << "\t\txStartOffset=" << smh.xStartOffset << endl;
  cout << "\t\tdimnames1StartOffset=" << smh.dimname1StartOffset << endl;
  cout << "\t\tdimnames2StartOffset=" << smh.dimname2StartOffset << endl;
  cout << "\t\tdimnames2EndOffset=" << smh.dimname2EndOffset << endl;

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
  smhData.write( matsparseDimnames1.c_str(), matsparseDimnames1.size());
  smhData.write( matsparseDimnames2.c_str(), matsparseDimnames2.size());

  // Convert the buffer to a string
  string smhDataString = smhData.str();

  struct entry* sparseMatrixEntry = make_entry_from_string("sparseMatrix", smhDataString);
  entries.push_back(*sparseMatrixEntry);
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

/**
 * Read a whole file from the disk
 */
string readWholeFile(string &filename) {
  ifstream in;
  in.open(filename, ifstream::in);
  // https://stackoverflow.com/questions/116038/what-is-the-best-way-to-read-an-entire-file-into-a-stdstring-in-c
  string data = static_cast<stringstream const&>(stringstream() << in.rdbuf()).str();
  in.close();

  return data;
}

void make_file_from_payload(list<entry> &entries, string &filename) {
  cout << "Making File from payload..." << endl;

  cout << "\tFile format information" << endl;
  cout << "\t\tIndex entry size is " << sizeof(indexEntry) << " bytes" << endl;
  cout << "\t\tFile header size is " << sizeof(fileHeader) << " bytes" << endl;
  

  // Open the file
  ofstream fs;
  fs.open(filename, ios::out | ios::binary);

  cout << "\tPreparing header..." << endl;
  // Generate a header
  struct fileHeader header;

  // Clear the memory to avoid rubbish data writen to the file
  memset(&header, 0, sizeof(header));
  
  strcpy(header.identifier, "pagoda2datafile");
  header.versionMajor = 1;
  header.versionMinor = 0;
  header.flags = 0xFFFF;

  header.blockSize = FILE_BLOCK_SIZE;
  header.headerSize = sizeof(struct fileHeader);
  header.indexSize = sizeof(indexEntry) * entries.size();

  // TODO if verbose
  cout << "\tTotal index size is: " << header.indexSize << " bytes" << endl;

  // Construct the index in memory
  cout << "\tConstructing index..." << endl;
  
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

  cout << "\tWriting header to file..." << endl;
  // Write the header
  fs.write((const char*) &header, sizeof(header));

  cout << "\tWriting index enties to file..." << endl;
  // Write the index entries
  for (list<indexEntry>::iterator iterator = indexEntries.begin(); iterator != indexEntries.end(); ++iterator) {
    fs.write((const char*) &(*iterator), sizeof(indexEntry));
  }

  // Write the content
  cout << "\tWriting the file contents..." << endl;
  int i = 0;
  for(list<entry>::iterator iterator = entries.begin(); iterator != entries.end(); ++iterator) {
    cout << "\t\tWriting entry " << i++;
    size_t s = iterator-> blockSize * FILE_BLOCK_SIZE;

    cout << " of size " <<  iterator->blockSize << " blocks (or " << s << " bytes)" << endl;
    void *entry = malloc(s); // memspace for entry as will be on disk
    if (entry == 0) {
      throw EX_MEM_ALLOC_FAIL;
    }
    
    memset(entry,  0, s); // fill with 0s
    memcpy(entry, (const char*) iterator->payload, iterator->size); // copy the payload
    
    // Write to file
    fs.write( (const char*) entry, s);

    // Free the entry buffer allocated in the loop
    free(entry);
  }

  cout << "Closing the output file" << endl;
  fs.close();
  
}

void makeAspectSparseMatrix(list<entry> &entries, string& indir) {
  cout << "Making aspect sparse matrix entry" << endl;
  
  // Read the p
  string mataspectPFile = indir + "mataspect_p.txt";
  list<uint32_t> *pData;
  cout << "\tReading mataspect_p.txt..." << endl;
  pData = readNumberArrayFromFile<uint32_t>(mataspectPFile);
  cout << "\t\tp array size: " << pData->size() << " [First entry value: " <<  pData->front() << "]" << endl;
  
  // Read the i
  string mataspectIFile = indir + "mataspect_i.txt";
  list<uint32_t> *iData;
  cout << "\tReading mataspect_i.txt..." << endl;
  iData = readNumberArrayFromFile<uint32_t>(mataspectIFile);
  cout << "\t\ti array size: " << iData->size() << " [First entry value: " << iData->front() << "]" << endl;
  
  // Read the x
  string mataspectXFile = indir + "mataspect_x.txt";
  list<float> *xData;
  cout << "\tReading " << mataspectXFile << endl;
  xData = readNumberArrayFromFile<float>(mataspectXFile);
  cout << "\t\tx array size: " << xData->size() << " [First entry value: " << xData->front() << "]" << endl;
  
  // Read dim names 1
  string mataspectDimnames1File = indir + "mataspect_Dimnames1.json";
  cout << "\tReading " <<  mataspectDimnames1File << endl;
  string mataspectDimnames1 = readWholeFile(mataspectDimnames1File);

  // Read dimnames 2
  string mataspectDimnames2File = indir + "mataspect_Dimnames2.json";
  cout << "\tReading " << mataspectDimnames2File << endl;
  string mataspectDimnames2 = readWholeFile(mataspectDimnames2File);

  string dimFile = indir + "mataspect_Dim.txt";
  list<uint32_t> *Dim;
  cout << "\tReading " << dimFile << endl;
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
  smh.dimname2StartOffset = smh.dimname1StartOffset + mataspectDimnames1.size();
  smh.dimname2EndOffset = smh.dimname2StartOffset + mataspectDimnames2.size();

  cout << "\tAspect matrix header information" << endl;
  cout << "\t\tdim1=" << smh.dim1 << endl;
  cout << "\t\tdim2=" << smh.dim2 << endl;
  cout << "\t\tpStartOffset=" << smh.pStartOffset << endl;
  cout << "\t\tiStartOffset=" << smh.iStartOffset << endl;
  cout << "\t\txStartOffset=" << smh.xStartOffset << endl;
  cout << "\t\tdimnames1StartOffset=" << smh.dimname1StartOffset << endl;
  cout << "\t\tdimnames2StartOffset=" << smh.dimname2StartOffset << endl;
  cout << "\t\tdimnames2EndOffset=" << smh.dimname2EndOffset << endl;
  
   // Make a memory holder for the data
  stringstream smhData(stringstream::in|stringstream::out|stringstream::binary);
  // Write the header
  smhData.write((const char*) &smh, sizeof(smh));
  
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
  smhData.write( mataspectDimnames1.c_str(), mataspectDimnames1.size());
  smhData.write( mataspectDimnames2.c_str(), mataspectDimnames2.size());

  // Convert the buffer to a string
  string smhDataString = smhData.str();

  // Make the entry and add it to the list
  struct entry* sparseMatrixEntry = make_entry_from_string("aspectMatrix", smhDataString);
  entries.push_back(*sparseMatrixEntry);
}
