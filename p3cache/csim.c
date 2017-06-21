#include "cachelab.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <limits.h>
#include <unistd.h>
#include <getopt.h>

// The block struct starts with an invalid tag (0) and LRU = 0, which is incremented
typedef struct block_s
{
	int valid;
	unsigned long long int tag;
	int LRU;
} block_s;

typedef struct set_s
{
	block_s *blocks;
} set_s;

typedef struct cache_s
{
	int s;
	int E;
	int b;
	set_s *sets;
} cache_s;

int helpmsg()
// Basic info printed when -h flag is present
{
	printf("Usage: ./test-csim [-hv] -s <num> -E <num> -b <num> -t <file>\n");
	printf("Options:\n");
	printf("-h		Print this help message.\n");
	printf("-v		Optional verbose flag.\n");
	printf("-s <num>	Number of set index bits.\n");
	printf("-E <num>	Number of lines per set.\n");
	printf("-b <num>	Number of block offset bits.\n");
	printf("-t <file>	Trace file.\n\n");
	printf("Examples:\n");
	printf("linux>	./test-csim -s 4 -E 1 -b 4 -t traces/yi.trace\n");
	printf("linux>	./test-csim -v -s 8 -E 2 -b 4 -t traces/yi.trace\n");
	return 0;
}

cache_s* create_cache(int s, int E, int b)
{
	int S = pow(2,s);
	cache_s *cache = (cache_s*)malloc(sizeof(cache_s));
	cache->sets = (set_s*)malloc(sizeof(set_s)*S);
	cache->s = s;
	cache->E = E;
	cache->b = b;
	// Creates the blocks within each set
	int i,j;
	for (i=0;i<S;i++)
		cache->sets[i].blocks = (block_s*)malloc(sizeof(block_s)*E);
	// Sets each block's valid bit to invalid and LRU to 0
	for (i=0;i<S;i++)
	{
		for (j=0;j<E;j++)
		{
			cache->sets[i].blocks[j].valid = 0;
			cache->sets[i].blocks[j].LRU = 0;
		}
	}
	return(cache);
}

void read_vars(int argc, char *argv[], int *s, int *E, int *b, char **trace, int *h, int *v)
// Every variable but argc and argv are outputs to be modified
{
	int c;
	while ((c = getopt(argc, argv, "sEbthv")) != -1)
		switch (c)
		{
		case 's': *s = atoi(argv[optind]);
			break;
		case 'E': *E = atoi(argv[optind]);
			break;
		case 'b': *b = atoi(argv[optind]);
			break;
		case 't': *trace = argv[optind];
			break;
		case 'h': *h = 1;
			break;
		case 'v': *v = 1;
			break;
		}
}

int load_store_tally(cache_s *cache, int address, int *hits, int *misses, int *evicts, int s, int b, int S, \
int E, int *LRU)
// Determines whether the address load/store is a hit/miss and if miss if it evicts too
// Returns -1 for evict, 0 for miss, 1 for hit
{
	// First determine vars (set num, tag num)
	int set = (address >> b) & (S - 1);
	int tag = (address >> (s + b));

	// Then loop through the set
	// In loop: check valid bit/tag num
	int i;
	for (i=0;i<E;i++)
	{
		// Is the valid bit set?
		if (cache->sets[set].blocks[i].valid == 1)
		{
			if (cache->sets[set].blocks[i].tag == tag)
			{
				// It's a hit!
				*hits = *hits + 1;
				cache->sets[set].blocks[i].LRU = *LRU;
				*LRU = *LRU + 1;
				return 1;
			}
		}
	}
	// If miss, check for evict and change cache value (LRU)
	int LRU_index = 0;
	int LRU_val = INT_MAX;
	// Logs the Least Recently Used cache block
	// Or if there is an invalid (open) block, fills it
	for (i=0;i<E;i++)
	{
		// If the miss is not an evict (open space found)
		if (cache->sets[set].blocks[i].valid == 0)
		{
			// A miss alas, but no eviction
			cache->sets[set].blocks[i].tag = tag;
			cache->sets[set].blocks[i].valid = 1;
			cache->sets[set].blocks[i].LRU = *LRU;
			*misses = *misses + 1;
			*LRU = *LRU + 1;
			return 0;
		}
		// Else log the LRU block for eviction
		else if (cache->sets[set].blocks[i].LRU < LRU_val)
		{
			LRU_index = i;
			LRU_val = cache->sets[set].blocks[i].LRU;
		}
		else;
	}
	// Evicts if a conflict miss
	cache->sets[set].blocks[LRU_index].tag = tag;
	cache->sets[set].blocks[LRU_index].LRU = *LRU;
	*misses = *misses + 1;
	*evicts = *evicts + 1;
	*LRU = *LRU + 1;
	return -1;
}

void norm_tally(cache_s *cache, char *trace, int *hits, int *misses, int *evicts)
// Performs the full cache test, line by line (without -v flag)
{
	FILE *fp;
	fp = fopen(trace,"r");
	char line[50];
	char cmd[5];
	unsigned long long addr = 0;
	int size;
	if (fp == NULL)
	{
		fprintf(stderr,"Error opening file");	
		return;
	}
	int s = cache->s;
	int b = cache->b;
	int E = cache->E;
	int S = pow(2,s);
	int LRU = 1;
	// Reads each line, one at a time, from file
	while (fgets(line,50,fp) != NULL)
	{
		// Scans string format
		sscanf(line," %s %llx,%d",cmd,&addr,&size);
		// if there is an instruction command, skip to next
		if (cmd[0] == 'I')
		{
			continue;
		}
		// Checks the operation.
		//Performs two for M, one for L/S 
		else if (cmd[0] == 'M')
		{
			// Data modify
			load_store_tally(cache, addr, hits, misses, evicts, s, b, S, E, &LRU);
			// since modify goes twice, the 2nd is a guranteed hit
			*hits = *hits + 1;
		}
		else
		{
			// Data load / Data store
			load_store_tally(cache, addr, hits, misses, evicts, s, b, S, E, &LRU);
		}
	}
}

int main(int argc, char *argv[])
{
	// Create variables for each flag, input, and the trace
	// Then fill them with read_vars
	int s, E, b;
	int h = 0;
	int v = 0;
	char *tracefile = (char*)malloc(sizeof(char)*50);
	read_vars(argc, argv, &s, &E, &b, &tracefile, &h, &v);

	// check if the help flag is set
	if (h == 1)
		return(helpmsg());

	/* The cache is initialized as a
	new data structure */
	cache_s *cache = create_cache(s,E,b);

	int hits = 0;
	int misses = 0;
	int evicts = 0;
	norm_tally(cache, tracefile, &hits, &misses, &evicts);   

	printSummary(hits, misses, evicts);
	return 0;
}
