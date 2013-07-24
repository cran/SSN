
#include <string>
#include <sstream>

#include <R.h>
#include <Rinternals.h>
#include <map>
#include <vector>
#include <cstring>
#include <ctype.h>

#include <boost/shared_ptr.hpp>
#include <algorithm>

struct Node
{
	int rid;
	const char* binaryID;
	boost::shared_ptr<Node> child1, child2, parent;
	double proportionalInfluence;
	double functionalValue;
	int index;
	Node()
		: functionalValue(-1)
	{}
};
typedef boost::shared_ptr<Node> NodePtr;

struct cmp_str
{
   bool operator()(char const *a, char const *b) const
   {
      return std::strcmp(a, b) < 0;
   }
};

struct Network
{
	std::vector<NodePtr> allNodes;
	typedef std::map<const char*, NodePtr, cmp_str> BinaryLookupMap;
	BinaryLookupMap binaryRepresentationsToNodes;
	NodePtr rootNode;
};
typedef boost::shared_ptr<Network> NetworkPtr;
extern "C"
{
	SEXP additiveFunctionValues(SEXP reachIDs, SEXP binaryIDs, SEXP proportionalInfluences, SEXP netIDs)
	{
		unsigned int nReachIDs = LENGTH(reachIDs);
		unsigned int nBinaryIDs = LENGTH(binaryIDs);
		unsigned int nProportionalInfluences = LENGTH(proportionalInfluences);
		unsigned int nNetIDs = LENGTH(netIDs);
		
		typedef std::map<int, NetworkPtr> NetworksMapType;
		std::vector<char*> allocatedStrings;
		NetworksMapType networks;
		
		if(nReachIDs != nBinaryIDs)
		{
			error("The number of input reach ID values must equal the number of input binary ID values\n");
			return R_NilValue;			
		}
		if(nBinaryIDs != nProportionalInfluences)
		{
			error("The number of input proportional influence values must equal the number of input binary ID values\n");
			return R_NilValue;			
		}
		if(nProportionalInfluences != nNetIDs)
		{
			error("The number of input proportional influence values must equal the number of input network ID values\n");
			return R_NilValue;
		}
		if(TYPEOF(reachIDs) != INTSXP)
		{
			error("Input reach ID values must be integers\n");
			return R_NilValue;
		}
		if(TYPEOF(binaryIDs) != STRSXP)
		{
			error("Input binary ID values must be character strings\n");
			return R_NilValue;
		}
		if(TYPEOF(proportionalInfluences) != REALSXP)
		{
			error("Input proportional influence values must be floating point numeric\n");
			return R_NilValue;
		}
		if(TYPEOF(netIDs) != INTSXP)
		{
			error("Input network ID values must be integers\n");
			return R_NilValue;
		}
		int* networkIDs_pointer = INTEGER(netIDs);
		int* rids_pointer = INTEGER(reachIDs);
		double* proportionalInfluences_pointer = REAL(proportionalInfluences);
		//the returned value
		SEXP result = R_NilValue;
		std::vector<int> sortedNetworkIDs(networkIDs_pointer, networkIDs_pointer + nNetIDs);
		
		std::sort(sortedNetworkIDs.begin(), sortedNetworkIDs.end());
		std::vector<int>::iterator unique_end = std::unique(sortedNetworkIDs.begin(), sortedNetworkIDs.end());
		
		//initialize an entry for every network. 
		for(std::vector<int>::iterator i = sortedNetworkIDs.begin(); i != unique_end; i++)
		{
			networks.insert(NetworksMapType::value_type(*i, NetworkPtr(new Network())));
		}
		bool hasError = false;	
		std::string errString;		
		//construct tree data structure
		for(unsigned int i = 0; i < nNetIDs; i++)
		{
			int networkID = networkIDs_pointer[i];
			const char* binaryRepresentation = CHAR(STRING_ELT(binaryIDs, i));
			int representationLength = strlen(binaryRepresentation);
			//if there are spaces at the beginning or end we need to trim the string. This means we need to malloc our own copy, so track this in allocatedStrings
			if(isspace(binaryRepresentation[0]) || isspace(binaryRepresentation[representationLength-1]))
			{
				const char* start = binaryRepresentation;
				while(isspace(*start) && *start != 0) start++;
				if(*start == 0)
				{
					hasError = true;
					errString = "Blank binary ID detected";
					goto cleanup;
				}
				else
				{	
					const char* end = binaryRepresentation + representationLength - 1;
					while(isspace(*end)) end--;
					representationLength = end - start + 1;
					char* representation = new char[representationLength+1];
					memcpy(representation, start, representationLength);
					//add null terminator
					representation[representationLength] = 0;
					allocatedStrings.push_back(representation);
					binaryRepresentation = representation;
				}
			}
			NetworkPtr currentNetwork = networks[networkID];
						
			NodePtr newNode = NodePtr(new Node());
			newNode->rid = rids_pointer[i];
			newNode->index = i;
			newNode->binaryID = binaryRepresentation;
			newNode->proportionalInfluence = proportionalInfluences_pointer[i];
			
			currentNetwork->allNodes.push_back(newNode);
			if(currentNetwork->binaryRepresentationsToNodes.find(binaryRepresentation) != currentNetwork->binaryRepresentationsToNodes.end())
			{
				hasError = true;
				errString = "Duplicate binary IDs found";
				goto cleanup;
			}
			currentNetwork->binaryRepresentationsToNodes.insert(Network::BinaryLookupMap::value_type(binaryRepresentation, newNode));
			
			//if it's the root don't bother looking for parents
			if(std::strcmp(binaryRepresentation, "1") != 0)
			{
				char* parentBinaryID = strdup(binaryRepresentation);
				parentBinaryID[representationLength-1] = 0;
				Network::BinaryLookupMap::iterator findParent = currentNetwork->binaryRepresentationsToNodes.find(parentBinaryID);
				if(findParent != currentNetwork->binaryRepresentationsToNodes.end())
				{
					//child one or child two?
					if(binaryRepresentation[representationLength-1] == '0')
					{
						findParent->second->child1 = newNode;
					}
					else
					{
						findParent->second->child2 = newNode;
					}
					newNode->parent = findParent->second;
				}
				free(parentBinaryID);
			}
			//if it's the root node, store it
			else 
			{
				currentNetwork->rootNode = newNode;
			}
			char* childID = new char[representationLength+2];
			strcpy(childID, binaryRepresentation);
			childID[representationLength] = '0';
			childID[representationLength+1] = 0;
			Network::BinaryLookupMap::iterator possibleChild1 = currentNetwork->binaryRepresentationsToNodes.find(childID);
			if(possibleChild1 != currentNetwork->binaryRepresentationsToNodes.end())
			{
				newNode->child1 = possibleChild1->second;
				possibleChild1->second->parent= newNode;
			}
			
			childID[representationLength] = '1';
			Network::BinaryLookupMap::iterator possibleChild2 = currentNetwork->binaryRepresentationsToNodes.find(childID);
			if(possibleChild2 != currentNetwork->binaryRepresentationsToNodes.end())
			{
				newNode->child2 = possibleChild2->second;
				possibleChild2->second->parent = newNode;
			}
			delete[] childID;
		}
		//normalize proportional influence values so that if we have two child nodes then their proportional influence values sum to 1
		for(NetworksMapType::iterator i = networks.begin(); i != networks.end(); i++)
		{
			NetworkPtr currentNetwork = i-> second;
			for(std::vector<NodePtr>::iterator j = currentNetwork->allNodes.begin(); j != currentNetwork->allNodes.end(); j++)
			{
				//if it's not a terminal node, normalize. 
				if((*j)->child1 && (*j)->child2)
				{
					double sum = (*j)->child1->proportionalInfluence + (*j)->child2->proportionalInfluence;
					(*j)->child1->proportionalInfluence /= sum;
					(*j)->child2->proportionalInfluence /= sum;
				}
				else if((*j)->child1)
				{
					(*j)->child1->proportionalInfluence = 1;
				}
				else if((*j)->child2)
				{
					(*j)->child2->proportionalInfluence = 1;
				}
				if(!(*j)->parent)
				{
					(*j)->proportionalInfluence = 1;
				}
			}
		}	
		//storage for results
		PROTECT(result = allocVector(REALSXP, nNetIDs));
		{
			double* result_pointer = REAL(result);
			//this next chunk is basically a by-hand tree-traversal algorithm. Ugly, but we need to limit any dependencies
			for(NetworksMapType::iterator i = networks.begin(); i != networks.end(); i++)
			{
				NetworkPtr currentNetwork = i->second;
				NodePtr currentNode = currentNetwork->rootNode;
				if(!currentNode)
				{
					std::stringstream ss;
					ss << "Root node not found for network "<< i->first;
					errString = ss.str();
					hasError = true;
					goto cleanup;
				}
				currentNode->functionalValue = currentNode->proportionalInfluence;
				result_pointer[currentNode->index] = currentNode->proportionalInfluence;
				if(fabs(result_pointer[currentNode->index]) > 1)
				{
					errString = "Internal error, additive function value was out of range";
					hasError = true;
					goto cleanup;
				}
				do
				{
					while(currentNode->child1 || (!currentNode->child1 && currentNode->child2))
					{
						if(currentNode->child1)
						{
							currentNode = currentNode->child1;
						}
						else
						{
							currentNode = currentNode->child2;
						}
						currentNode->functionalValue = currentNode->proportionalInfluence * currentNode->parent->functionalValue;
						result_pointer[currentNode->index] = currentNode->functionalValue;
						if(fabs(result_pointer[currentNode->index]) > 1)
						{
							errString = "Internal error, additive function value was out of range";
							hasError = true;
							goto cleanup;
						}
					}
					do
					{
						if(!currentNode->parent) 
						{
							//we've finished for this network, so break out to the end of the for loop
							goto finish;
						}
						if(currentNode->parent->child1 && currentNode->parent->child1 == currentNode) 
						{
							if(currentNode->parent->child2)
							{
								currentNode = currentNode->parent->child2;
								currentNode->functionalValue = currentNode->proportionalInfluence * currentNode->parent->functionalValue;
								result_pointer[currentNode->index] = currentNode->functionalValue;
								if(fabs(result_pointer[currentNode->index]) > 1)
								{
									errString = "Internal error, additive function value was out of range";
									hasError = true;
									goto cleanup;
								}
								//we've finished this loop but we want to keep going on this network. So just break
								break;
							}
							else currentNode = currentNode->parent;
						}
						else currentNode = currentNode->parent;
					}
					while(true);
				}
				while(true);
			finish:
				;
			}
		}
		UNPROTECT(1);
	cleanup:
		//cleanup code goes in a seperate loop, in case we break out of the previous one. 
		for(NetworksMapType::iterator i = networks.begin(); i != networks.end(); i++)
		{
			NetworkPtr currentNetwork = i->second;
			//clean everything up
			for(std::vector<NodePtr>::iterator j = currentNetwork->allNodes.begin(); j != currentNetwork->allNodes.end(); j++)
			{
				(*j)->parent.reset();
				(*j)->child1.reset();
				(*j)->child2.reset();
			}
			currentNetwork->allNodes.clear();
			currentNetwork->rootNode.reset();
		}
		for(std::vector<char*>::iterator i = allocatedStrings.begin(); i != allocatedStrings.end(); i++) delete[] *i;
		if(hasError)
		{
			error(errString.c_str());
			return R_NilValue;
		}
		return result;
	}
}
