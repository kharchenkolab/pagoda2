/**
 * Filename: binaryExport.cpp
 * Description: Implements exporting to file via Rcpp
 * Date: August 6th 2017
 */

#include "pagoda2.h"
#include "binaryExport.h"
#include <chrono>

using namespace std;
using namespace std::chrono;

void addSparseMatrixToEntries(List spML,
                              list<entry> &entries,
                              char const *mattype,
                              binaryExportParams *params)
{


  auto t1 = high_resolution_clock::now();
  IntegerVector viData = spML["matsparse_i"];
  list<uint32_t> *iData;
  iData = IVtoL<uint32_t>(viData);
  auto t2 = high_resolution_clock::now();
  auto duration = duration_cast<microseconds>(t2 - t1).count();
  if (params->verboseTimings)
	       Rcpp::Rcout << "iData IVtoL took: " << duration << "us" << endl << flush;

  // Dim from R-export list and convert to list of pointers
  t1 = high_resolution_clock::now();
  IntegerVector vDim = spML["matsparse_dim"];
  list<uint32_t> *Dim;
  Dim = IVtoL<uint32_t>(vDim);
  t2 = high_resolution_clock::now();
  duration = duration_cast<microseconds>(t2 - t1).count();
  if (params->verboseTimings)
               Rcpp::Rcout << "spML IVtoL took: " << duration	<< "us" << endl << flush;

  // pData from R-export list and convert to list of pointers
  t1 = high_resolution_clock::now();
  IntegerVector vpData = spML["matsparse_p"];
  list<uint32_t> *pData;
  pData = IVtoL<uint32_t>(vpData);
  t2 = high_resolution_clock::now();
  duration = duration_cast<microseconds>(t2 - t1).count();
  if (params->verboseTimings)
               Rcpp::Rcout << "vpData IVtoL took: " << duration  << "us" << endl << flush;

  // xData from R-export list and convert to list of pointers
  t1 = high_resolution_clock::now();
  NumericVector vxData = spML["matsparse_x"];
  list<float> *xData;
  xData = NVtoL<float>(vxData);
  t2 = high_resolution_clock::now();
  duration = duration_cast<microseconds>(t2 - t1).count();
  if (params->verboseTimings)
               Rcpp::Rcout << "xData IVtoL took: " << duration  << "us" << endl << flush;
  
  if (params->verbose)
  {
    Rcpp::Rcout << "\t\tp array size: " << pData->size() << " [First entry value: " << pData->front() << "]" << endl;
    Rcpp::Rcout << "\t\ti array size: " << iData->size() << " [First entry value: " << iData->front() << "]" << endl;
    Rcpp::Rcout << "\t\tx array size: " << xData->size() << " [First entry value: " << xData->front() << "]" << endl;
  }

  // Read the dimnames of the sparse matrices.
  // Add a Nul to the end, because thats what happened in writing to a temporary txt file in R
  string matsparseDimnames1 = spML["matsparse_dimnames1"];
  matsparseDimnames1.push_back('\0');

  string matsparseDimnames2 = spML["matsparse_dimnames2"];
  matsparseDimnames2.push_back('\0');

  // Create Header for sparse Matrix
  // read Dimensions of Matrix by iterating through the list Dim
  struct sparseMatrixHeader smh;
  list<uint32_t>::iterator li = Dim->begin();
  smh.dim1 = *li;
  li++;
  smh.dim2 = *li;

  smh.pStartOffset = sizeof(struct sparseMatrixHeader);
  smh.iStartOffset = smh.pStartOffset + sizeof(uint32_t) * pData->size();
  smh.xStartOffset = smh.iStartOffset + sizeof(uint32_t) * iData->size();
  smh.dimname1StartOffset = smh.xStartOffset + sizeof(uint32_t) * xData->size();
  smh.dimname2StartOffset = smh.dimname1StartOffset + matsparseDimnames1.size();
  smh.dimname2EndOffset = smh.dimname2StartOffset + matsparseDimnames2.size();

  if (params->verbose)
  {
    Rcpp::Rcout << "\tSparse matrix header information" << endl;
    Rcpp::Rcout << "\t\tdim1=" << smh.dim1 << endl;
    Rcpp::Rcout << "\t\tdim2=" << smh.dim2 << endl;
    Rcpp::Rcout << "\t\tpStartOffset=" << smh.pStartOffset << endl;
    Rcpp::Rcout << "\t\tiStartOffset=" << smh.iStartOffset << endl;
    Rcpp::Rcout << "\t\txStartOffset=" << smh.xStartOffset << endl;
    Rcpp::Rcout << "\t\tdimnames1StartOffset=" << smh.dimname1StartOffset << endl;
    Rcpp::Rcout << "\t\tdimnames2StartOffset=" << smh.dimname2StartOffset << endl;
    Rcpp::Rcout << "\t\tdimnames2EndOffset=" << smh.dimname2EndOffset << endl;
  }

  // Make an in-memorty string stream to hold the data
  stringstream smhData(stringstream::in | stringstream::out | stringstream::binary);

  // Write the header
  smhData.write((const char *)&smh, sizeof(smh));

  // Write the p object

  for (list<uint32_t>::const_iterator iter = pData->begin(); iter != pData->end(); ++iter)
  {
    smhData.write((const char *)&*iter, sizeof(uint32_t));
  }
  

  // Write the i object
  for (list<uint32_t>::const_iterator iter = iData->begin(); iter != iData->end(); ++iter)
  {
    smhData.write((const char *)&*iter, sizeof(uint32_t));
  }

  // Write the x object
  for (list<float>::const_iterator iter = xData->begin(); iter != xData->end(); ++iter)
  {
    smhData.write((const char *)&*iter, sizeof(uint32_t));
  }

  // Clear pData, iData and xData
  delete pData;
  delete iData;
  delete xData;

  // Write the Dimnames as JSON string
  smhData.write(matsparseDimnames1.c_str(), matsparseDimnames1.size());
  smhData.write(matsparseDimnames2.c_str(), matsparseDimnames2.size());

  // Convert the buffer to a string
  string smhDataString = smhData.str();

  struct entry *sparseMatrixEntry = make_entry_from_string(mattype, smhDataString);
  entries.push_back(*sparseMatrixEntry);

  // TODO: Memory manage the sparseMatrixEntry memory
}







/**
 * Rcpp function called from R to write the web object
 * @description accepts a list of items to write and the name of a file to write to
 */
// [[Rcpp::export]]
void WriteListToBinary(List expL, std::string outfile,bool verbose=false)
{
  // Define a parameter set
    struct binaryExportParams params;

    params.verbose = verbose; // pass as a reference (&) to called  functions
    params.verboseTimings = false;

    if(params.verboseTimings) 
      Rcpp::Rcout << "Entering C++ code section" << endl << flush;
    
    CharacterVector elements = expL.names();

    // Structure list<entry> as defined in pagoda2.h - List for all entries
    list<entry> entries;

    // Optional Gene KNN inclusion if present
    if(params.verboseTimings) Rcpp::Rcout << "Start Gene KNN optional inclusion" << endl << flush;
    auto t1 = high_resolution_clock::now();
    if(std::find(elements.begin(),elements.end(),"geneknn") != elements.end()) {
      // JSON formatted gene knn and make entries in payload:
      string geneKnn = expL["geneknn"];
      struct entry *geneKnnEntry = make_entry_from_string("geneknn", geneKnn);
      entries.push_back(*geneKnnEntry);
    }
    auto t2 = high_resolution_clock::now();
    auto duration = duration_cast<microseconds>(t2 - t1).count();
    if(params.verboseTimings) 
      Rcpp::Rcout << "Finding and including geneknn object: " << duration << " ms" <<endl << flush;
  
    
    // Read in JSON formatted strings from R List given by List expL
    string cellmetadataData = expL["cellmetadata"];
    string cellorderData = expL["cellorder"];
    string geneinformationData = expL["geneinformation"];
    string reduceddendrogramData = expL["reduceddendrogram"];
    string embeddingstructureData = expL["embeddingstructure"];
    string aspectInformationData = expL["aspectInformation"];
    string genesetsData = expL["genesets"];
    string genesetsgenesData = expL["genesetGenes"];
    string appmetadata = expL["appmetadata"];

    // Reading in the names of exported Embeddings:
    vector<string> embedList = expL["embedList"];

    //Make entries for JSON items

    // Application metadata
    struct entry *appmetadataEntry = make_entry_from_string("appmetadata", appmetadata);
    entries.push_back(*appmetadataEntry);

    // Cellmetadata
    struct entry *metadataEntry = make_entry_from_string("cellmetadata", cellmetadataData);
    entries.push_back(*metadataEntry);
    // Cellorder
    struct entry *cellorderEntry = make_entry_from_string("cellorder", cellorderData);
    entries.push_back(*cellorderEntry);
    // Geneinformation
    struct entry *geneinformationEntry = make_entry_from_string("geneinformation", geneinformationData);
    entries.push_back(*geneinformationEntry);
    // Reduced Dendrogram
    struct entry *reduceddendrogramEntry = make_entry_from_string("reduceddendrogram", reduceddendrogramData);
    entries.push_back(*reduceddendrogramEntry);
    // EmbeddingStructure
    struct entry *embeddingstructureEntry = make_entry_from_string("embeddingstructure", embeddingstructureData);
    entries.push_back(*embeddingstructureEntry);
    // - Aspect Information
    struct entry *aspectInformationEntry = make_entry_from_string("aspectinformation", aspectInformationData);
    entries.push_back(*aspectInformationEntry);
    // - Genesets Data
    struct entry *genesetsEntry = make_entry_from_string("genesets", genesetsData);
    entries.push_back(*genesetsEntry);
    // - Genesets-genes Data
    struct entry *genesetsgenesEntry = make_entry_from_string("genesetsgenes", genesetsgenesData);
    entries.push_back(*genesetsgenesEntry);

    // Push the embeddings
    for (int embedIndex = 0; embedIndex != embedList.size(); ++embedIndex)
    {
        // TODO: Remove the json-extension - should be removed in the R part
        string AembedName = embedList[embedIndex];
        size_t lastindex = AembedName.find_last_of(".");
        string embedName = AembedName.substr(0, lastindex);
        string embData = expL[AembedName];
        struct entry *embEntry = make_entry_from_string(embedName.c_str(), embData);
        entries.push_back(*embEntry);
    }



    // Write sparse expression Matrix to payload:
//exportList[["sparsematnames"]] <- c("matsparse", "mataspect");
    vector<string> sparseMatrixNames = expL["sparsematnames"];
    for (auto it = sparseMatrixNames.begin(); it != sparseMatrixNames.end(); ++it) {
      if (params.verbose)
        Rcpp::Rcout << "Writing... " << *it << endl << flush;

      // Small hack to match internal and frontend names
      // TODO: fix front end and update with backwards compatibility
      string saveName;
      if (*it == "matsparse") {
        saveName = "sparseMatrix";
      } else if (*it == "mataspect") {
        saveName = "aspectMatrix";
      } else {
        saveName = *it;
      }

      addSparseMatrixToEntries(expL[*it], entries, saveName.c_str(), &params);
    }

    //addSparseMatrixToEntries(expL["matsparse"], entries, "sparseMatrix", &params);
    //addSparseMatrixToEntries(expL["mataspect"], entries, "aspectMatrix", &params);

    // If sparse aspect matrix exists in the passed List write sparse aspect Matrix to payload:
    // if (std::find(elements.begin(), elements.end(), "mataspect") != elements.end()) {
    //   addSparseMatrixToEntries(expL["mataspect"], entries, *"aspectMatrix", &params);
    // }

      // Writing file
      if (params.verbose)
      {
        Rcpp::Rcout << "Making File from payload..." << endl;

        Rcpp::Rcout << "\tFile format information" << endl;
        Rcpp::Rcout << "\t\tIndex entry size is " << sizeof(indexEntry) << " bytes" << endl;
        Rcpp::Rcout << "\t\tFile header size is " << sizeof(fileHeader) << " bytes" << endl;
      }
      // Export entries to file
      ofstream fs;
      fs.open(outfile, ios::out | ios::binary);

      if (params.verbose)
      {
        Rcpp::Rcout << "\tPreparing header..." << endl;
      }
      struct fileHeader header;

      // Clear the memory to avoid rubbish data written to the file
      memset(&header, 0, sizeof(header));

      strcpy(header.identifier, "pagoda2datafile");
      header.versionMajor = 1;
      header.versionMinor = 0;
      header.flags = 0xFFFF;

      header.blockSize = FILE_BLOCK_SIZE;
      header.headerSize = sizeof(struct fileHeader);
      header.indexSize = sizeof(indexEntry) * entries.size();

      if (params.verbose)
      {
        // TODO if verbose
        Rcpp::Rcout << "\tTotal index size is: " << header.indexSize << " bytes" << endl;

        // Construct the index in memory
        Rcpp::Rcout << "\tConstructing index..." << endl;
      }

      list<indexEntry> indexEntries;
      uint32_t curOffset = 0; // in blocks

      // Calculate the index entry sizes
      for (list<entry>::iterator iterator = entries.begin(); iterator != entries.end(); ++iterator)
      {
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

    ///////////////////////////////////////////
    // Write the file to disk
    ///////////////////////////////////////////

    // Write the header
    fs.write((const char *) &header, sizeof(header));

    // Write the index entries
    for (list<indexEntry>::iterator iterator = indexEntries.begin(); iterator != indexEntries.end(); ++iterator)
    {
        fs.write((const char *)&(*iterator), sizeof(indexEntry));
    }

    // Write the file content
    int i = 0;
    for (list<entry>::iterator iterator = entries.begin(); iterator != entries.end(); ++iterator)
    {

        size_t s = iterator->blockSize * FILE_BLOCK_SIZE;

        if (params.verbose) {
          Rcpp::Rcout << "\t\tWriting entry " << i++;
          Rcpp::Rcout << " of size " << iterator->blockSize << " blocks (or " << s << " bytes)" << endl;
        }

        void *entry = malloc(s);
        if (entry == 0)
        {
            throw EX_MEM_ALLOC_FAIL;
        }

        // fill with 0s
        memset(entry, 0, s);
        // copy the payload
        memcpy(entry, (const char *)iterator->payload, iterator->size);

        fs.write((const char *)entry, s);

        free(entry);
    }
    fs.close();

    // Free up the payloads of the entries
    if (params.verbose) {
      Rcpp::Rcout << "Free up entry payloads" << endl;
    }
    for (list<entry>::iterator iterator = entries.begin(); iterator != entries.end(); ++iterator)
    {
      free(iterator->payload);
    }
}


///////////////////////////////////
// Helper functions
///////////////////////////////////

/**
 * Helper function for getting rcpp data in
 */
template <class T>
std::list<T>* IVtoL(Rcpp::IntegerVector f)
{
  std::list<T>* s;
  s = new list<T>;

  for (int i = 0; i < f.size(); i++)
  {
    T val(f[i]);
    s->push_back(val);
  }

  return (s);
}


/**
 * Helper function for getting rcpp data in
 */
template <class T>
std::list<T>* NVtoL(Rcpp::NumericVector f)
{
  std::list<T>* s;
  s = new list<T>;

  for (int i = 0; i < f.size(); i++)
  {
    T val(f[i]);
    s->push_back(val);
  }
  return (s);
}


/**
 * Create the internal entry structure from a key and data
 * @description allocates space for the new entry structure, calculates required values
 * and allocates space for the data and copies it there
 */
struct entry *make_entry_from_string(char const *key, string &data)
{
  // Allocate space for the structure
  struct entry *e;
  e = (struct entry *)malloc(sizeof(struct entry));
  if (e == 0)
  {
    throw EX_MEM_ALLOC_FAIL;
  }

  // Set memory to all zeros
  memset(e, 0, sizeof(entry));

  // Set the key of the entry
  strcpy(e->key, key);

  // Set the length field
  uint64_t entryLengthBytes = data.length();

  // Allocate memory for the payload
  e->payload = malloc(entryLengthBytes);
  if (e->payload == 0)
  {
    throw EX_MEM_ALLOC_FAIL;
  }

  // Copy the payload to the entry
  memcpy(e->payload, data.c_str(), entryLengthBytes);

  // Calculate the number of blocks that will be required
  e->size = entryLengthBytes;
  e->blockSize = (uint32_t)intDivRoundUP(entryLengthBytes, FILE_BLOCK_SIZE);

  // Return pointer to the newly allocated entry
  return e;
}

