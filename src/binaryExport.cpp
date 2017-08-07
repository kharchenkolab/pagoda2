/**
 * Filename: binaryExport.cpp
 * Description: Implements exporting to file via Rcpp
 * Date: August 6th 2017
 */




#include "pagoda2.h"
#include "binaryExport.h"


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



/**
 * Rcpp function called from R to write the web object
 * @description accepts a list of items to write and the name of a file to write to
 */
// [[Rcpp::export]]
void WriteListToBinary(List expL, std::string outfile)
{
    // Read in JSON formatted strings from R List given by List expL
    // TODO: Move the "convert to JSON" into a Cpp function
    string cellmetadataData = expL["cellmetadata"];
    string cellorderData = expL["cellorder"];
    string geneinformationData = expL["geneinformation"];
    string reduceddendrogramData = expL["reduceddendrogram"];
    string embeddingstructureData = expL["embeddingstructure"];
    string aspectInformationData = expL["aspectInformation"];
    string genesetsData = expL["genesets"];
    string genesetsgenesData = expL["genesetGenes"];

    // Reading in the names of exported Embeddings:
    vector<string> embedList = expL["embedList"];

    // Structure list<entry> as defined in pagoda2.h - List for all entries
    list<entry> entries;

    // Make Entry from each JSON string passed by expL
    // - Cellmetadata:
    struct entry *metadataEntry = make_entry_from_string("cellmetadata", cellmetadataData);
    entries.push_back(*metadataEntry);

    // - Cellorder:
    struct entry *cellorderEntry = make_entry_from_string("cellorder", cellorderData);
    entries.push_back(*cellorderEntry);

    // - Geneinformation:
    struct entry *geneinformationEntry = make_entry_from_string("geneinformation", geneinformationData);
    entries.push_back(*geneinformationEntry);

    // - Reduced Dendrogram
    struct entry *reduceddendrogramEntry = make_entry_from_string("reduceddendrogram", reduceddendrogramData);
    entries.push_back(*reduceddendrogramEntry);

    // - EmbeddingStructure
    struct entry *embeddingstructureEntry = make_entry_from_string("embeddingstructure", embeddingstructureData);
    entries.push_back(*embeddingstructureEntry);

    // Reading all exported Embeddings into entries. Iteration over embedList
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

    // ------------------------  Sparse Count Matrix  ------------------------
    // Read in iData from R-export list and convert to list of pointers
    IntegerVector viData = expL["matsparse_i"];
    list<uint32_t> *iData;
    iData = IVtoL<uint32_t>(viData);

    // Dim from R-export list and convert to list of pointers
    IntegerVector vDim = expL["matsparse_dim"];
    list<uint32_t> *Dim;
    Dim = IVtoL<uint32_t>(vDim);

    // pData from R-export list and convert to list of pointers
    IntegerVector vpData = expL["matsparse_p"];
    list<uint32_t> *pData;
    pData = IVtoL<uint32_t>(vpData);

    // xData from R-export list and convert to list of pointers
    NumericVector vxData = expL["matsparse_x"];
    list<float> *xData;
    xData = NVtoL<float>(vxData);

    cout << "\t\tp array size: " << pData->size() << " [First entry value: " << pData->front() << "]" << endl;
    cout << "\t\ti array size: " << iData->size() << " [First entry value: " << iData->front() << "]" << endl;
    cout << "\t\tx array size: " << xData->size() << " [First entry value: " << xData->front() << "]" << endl;


    // Read the dimnames of the sparse matrices.
    // Add a Nul to the end, because thats what happened in writing to a temporary txt file in R
    string matsparseDimnames1 = expL["matsparse_dimnames1"];
    matsparseDimnames1.push_back('\0');
    string matsparseDimnames2 = expL["matsparse_dimnames2"];
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

    cout << "\tExpression matrix header information" << endl;
    cout << "\t\tdim1=" << smh.dim1 << endl;
    cout << "\t\tdim2=" << smh.dim2 << endl;
    cout << "\t\tpStartOffset=" << smh.pStartOffset << endl;
    cout << "\t\tiStartOffset=" << smh.iStartOffset << endl;
    cout << "\t\txStartOffset=" << smh.xStartOffset << endl;
    cout << "\t\tdimnames1StartOffset=" << smh.dimname1StartOffset << endl;
    cout << "\t\tdimnames2StartOffset=" << smh.dimname2StartOffset << endl;
    cout << "\t\tdimnames2EndOffset=" << smh.dimname2EndOffset << endl;


    // Make a memory holder for the data
    stringstream smhData(stringstream::in | stringstream::out | stringstream::binary);

    // Write the header
    smhData.write((const char *)&smh, sizeof(smh));

    // Write the p object
    for (list<uint32_t>::const_iterator iter = pData->begin(); iter != pData->end(); ++iter)
    {
        smhData.write((const char *) &*iter, sizeof(uint32_t));
    }

    // Write the i object
    for (list<uint32_t>::const_iterator iter = iData->begin(); iter != iData->end(); ++iter)
    {
        smhData.write((const char *) &*iter, sizeof(uint32_t));
    }

    // Write the x object
    for (list<float>::const_iterator iter = xData->begin(); iter != xData->end(); ++iter)
    {
        smhData.write((const char *) &*iter, sizeof(uint32_t));
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

    struct entry *sparseMatrixEntry = make_entry_from_string("sparseMatrix", smhDataString);
    entries.push_back(*sparseMatrixEntry);

    // ------------------------  Sparse Aspect Matrix  ------------------------
    // Read in iData from R-export list and convert to list of pointers
    IntegerVector AviData = expL["mataspect_i"];
    list<uint32_t> *AiData;
    AiData = IVtoL<uint32_t>(AviData);

    // Read in pData from R-export list and convert to list of pointers
    IntegerVector AvpData = expL["mataspect_p"];
    list<uint32_t> *ApData;
    ApData = IVtoL<uint32_t>(AvpData);

    // Read in Dim from R-export list and convert to list of pointers
    IntegerVector AvDim = expL["mataspect_dim"];
    list<uint32_t> *ADim;
    ADim = IVtoL<uint32_t>(AvDim);

    // Read in xData from R-export list and convert to list of pointers
    NumericVector AvxData = expL["mataspect_x"];
    list<float> *AxData;
    AxData = NVtoL<float>(AvxData);

    cout << "\t\tp array size: " << ApData->size() << " [First entry value: " << ApData->front() << "]" << endl;
    cout << "\t\ti array size: " << AiData->size() << " [First entry value: " << AiData->front() << "]" << endl;
    cout << "\t\tx array size: " << AxData->size() << " [First entry value: " << AxData->front() << "]" << endl;

    // Read the dimnames of the sparse matrices.
    // Add a Nul to the end, because thats what happened in writing to a temporary txt file in R
    string mataspectDimnames1 = expL["mataspect_dimnames1"];
    mataspectDimnames1.push_back('\0');
    string mataspectDimnames2 = expL["mataspect_dimnames2"];
    mataspectDimnames2.push_back('\0');

    // Create Header for sparse Aspect Matrix
    // read Dimensions of Matrix by iterating through the list Dim
    struct sparseMatrixHeader Asmh;
    list<uint32_t>::iterator Ali = ADim->begin();
    Asmh.dim1 = *Ali;
    Ali++;
    Asmh.dim2 = *Ali;

    Asmh.pStartOffset = sizeof(struct sparseMatrixHeader);
    Asmh.iStartOffset = Asmh.pStartOffset + sizeof(uint32_t) * ApData->size();
    Asmh.xStartOffset = Asmh.iStartOffset + sizeof(uint32_t) * AiData->size();
    Asmh.dimname1StartOffset = Asmh.xStartOffset + sizeof(uint32_t) * AxData->size();
    Asmh.dimname2StartOffset = Asmh.dimname1StartOffset + mataspectDimnames1.size();
    Asmh.dimname2EndOffset = Asmh.dimname2StartOffset + mataspectDimnames2.size();

    cout << "\tAspect matrix header information" << endl;
    cout << "\t\tdim1=" << Asmh.dim1 << endl;
    cout << "\t\tdim2=" << Asmh.dim2 << endl;
    cout << "\t\tpStartOffset=" << Asmh.pStartOffset << endl;
    cout << "\t\tiStartOffset=" << Asmh.iStartOffset << endl;
    cout << "\t\txStartOffset=" << Asmh.xStartOffset << endl;
    cout << "\t\tdimnames1StartOffset=" << Asmh.dimname1StartOffset << endl;
    cout << "\t\tdimnames2StartOffset=" << Asmh.dimname2StartOffset << endl;
    cout << "\t\tdimnames2EndOffset=" << Asmh.dimname2EndOffset << endl;

    // Make a memory holder for the data
    stringstream AsmhData(stringstream::in | stringstream::out | stringstream::binary);

    // Write the header
    AsmhData.write((const char *) &Asmh, sizeof(Asmh));

    // Write the p object
    for (list<uint32_t>::const_iterator iter = ApData->begin(); iter != ApData->end(); ++iter)
    {
        AsmhData.write((const char *) &*iter, sizeof(uint32_t));
    }

    // Write the i object
    for (list<uint32_t>::const_iterator iter = AiData->begin(); iter != AiData->end(); ++iter)
    {
        AsmhData.write((const char *) &*iter, sizeof(uint32_t));
    }

    // Write the x object
    for (list<float>::iterator iter = AxData->begin(); iter != AxData->end(); ++iter)
    {
        AsmhData.write((const char *) &*iter, sizeof(uint32_t));
    }

    // Write the Dimnames as JSON string
    AsmhData.write(mataspectDimnames1.c_str(), mataspectDimnames1.size());
    AsmhData.write(mataspectDimnames2.c_str(), mataspectDimnames2.size());

    // Convert the buffer to a string
    string AsmhDataString = AsmhData.str();

    struct entry *AsparseMatrixEntry = make_entry_from_string("aspectMatrix", AsmhDataString);
    entries.push_back(*AsparseMatrixEntry);
    // ------------------------   ------------------------

    // - Aspect Information
    struct entry *aspectInformationEntry = make_entry_from_string("aspectinformation", aspectInformationData);
    entries.push_back(*aspectInformationEntry);

    // - Genesets Data///////////////////////////////////////////
    struct entry *genesetsEntry = make_entry_from_string("genesets", genesetsData);
    entries.push_back(*genesetsEntry);

    // - Genesets-genes Data
    struct entry *genesetsgenesEntry = make_entry_from_string("genesetsgenes", genesetsgenesData);
    entries.push_back(*genesetsgenesEntry);



    // Writing file
    cout << "Making File from payload..." << endl;

    cout << "\tFile format information" << endl;
    cout << "\t\tIndex entry size is " << sizeof(indexEntry) << " bytes" << endl;
    cout << "\t\tFile header size is " << sizeof(fileHeader) << " bytes" << endl;

    // Export entries to file
    ofstream fs;
    fs.open(outfile, ios::out | ios::binary);

    cout << "\tPreparing header..." << endl;
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

    // TODO if verbose
    cout << "\tTotal index size is: " << header.indexSize << " bytes" << endl;

    // Construct the index in memory
    cout << "\tConstructing index..." << endl;

    list<indexEntry> indexEntries;
    uint32_t curOffset = 0; // in blocks

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
        cout << "\t\tWriting entry " << i++;
        size_t s = iterator->blockSize * FILE_BLOCK_SIZE;
        cout << " of size " << iterator->blockSize << " blocks (or " << s << " bytes)" << endl;
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
    cout << "Free up entry payloads" << endl;
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
