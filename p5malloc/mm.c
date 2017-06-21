/*
 * mm.c
 *
 * This is the only file you should modify.
 */
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mm.h"
#include "memlib.h"

/* If you want debugging output, use the following macro.  When you hand
 * in, remove the #define DEBUG line. */
#define DEBUG
#ifdef DEBUG
# define dbg_printf(...) printf(__VA_ARGS__)
#else
# define dbg_printf(...)
#endif

/* single word (8) or double word (16) alignment */
#define ALIGNMENT 8

/* rounds up to the nearest multiple of ALIGNMENT */
#define ALIGN(p) (((size_t)(p) + (ALIGNMENT-1)) & ~0x7)

/* $begin mallocmacros */
/* Basic constants and macros */
#define WSIZE       4       /* word size (bytes) */
#define DSIZE       8       /* doubleword size (bytes) */
#define CHUNKSIZE  (1<<12)  /* initial heap size (bytes) */
#define OVERHEAD    8       /* overhead of header and footer (bytes) */

#define MAX(x, y) ((x) > (y)? (x) : (y))

/* Pack a size and allocated bit into a word */
#define PACK(size, alloc)  ((size) | (alloc))

/* Read and write a word at address p */
/* NB: this code calls a 32-bit quantity a word */
#define GET(p)       (*(unsigned int *)(p))
#define PUT(p, val)  (*(unsigned int *)(p) = (val))
#define PUTVOID(p, val)  (*(void**)(p) = (val))
#define GETVOID(p)   (*(void**)(p))

/* Read the size and allocated fields from address p */
#define GET_SIZE(p)  (GET(p) & ~0x7)
#define GET_ALLOC(p) (GET(p) & 0x1)

/* Given block ptr bp, compute address of its header and footer */
#define HDRP(bp)       ((char *)(bp) - WSIZE)
#define FTRP(bp)       ((char *)(bp) + GET_SIZE(HDRP(bp)) - DSIZE)

/* Given block ptr bp, compute address of next and previous blocks */
#define NEXT_BLKP(bp)  ((char *)(bp) + GET_SIZE(((char *)(bp) - WSIZE)))
#define PREV_BLKP(bp)  ((char *)(bp) - GET_SIZE(((char *)(bp) - DSIZE)))

/* Given block ptr bp, compute address of next and prev free blocks
 * USING EXPLICIT LINKED LISTS */
#define NEXT_FBLKP(bp)  (*(char**)(bp))
#define PREV_FBLKP(bp)  (*((char**)(bp+DSIZE)))

/* $end mallocmacros */

/* Global variables */
static char *heap_listp;  /* pointer to first block */
static char *free_list;

/* function prototypes for internal helper routines */
static void *extend_heap(size_t words);
static void place(void *bp, size_t asize);
static void *find_fit(size_t asize);
static void *coalesce(void *bp);
static void insert_fb(void *bp);
static void remove_fb(void *bp);
static void printblock(void *bp);
static void checkblock(void *bp);
//static void print_freelist();

/*
 * Initialize: return -1 on error, 0 on success.
 */
int mm_init(void) {
  free_list = NULL;
  heap_listp = 0;

 /* create the initial empty heap */

  if ((heap_listp = mem_sbrk(8*WSIZE)) == NULL)
    return -1;
  PUT(heap_listp, 0);                        /* alignment padding */
  PUT(heap_listp+WSIZE, PACK(OVERHEAD+16, 1));  /* prologue header */
  PUT(heap_listp+DSIZE, 0);
  PUT(heap_listp+(2*DSIZE), 0);
  PUT(heap_listp+(3*DSIZE), PACK(OVERHEAD+16, 1));  /* prologue footer */
  PUT(heap_listp+(3*DSIZE)+WSIZE, PACK(0, 1));   /* epilogue header */
  heap_listp += (2*DSIZE);

  /* Extend the empty heap with a free block of CHUNKSIZE bytes */
  if (extend_heap(CHUNKSIZE/WSIZE) == NULL)
    return -1;
  return 0;
}

/*
 * malloc
 */
void *mm_malloc (size_t size) {

  size_t asize;      /* adjusted block size */
  size_t extendsize; /* amount to extend heap if no fit */
  char *bp;	     /* block pointer */

  if (heap_listp == 0){
    mm_init();
  }


  /* Ignore spurious requests */

  if (size <= 0)
    return NULL;

  /* Adjust block size to include overhead and alignment reqs. */

  if (size <= (2*DSIZE))
    asize = (2*DSIZE) + OVERHEAD;
  else
    asize = OVERHEAD + ALIGN(size);

  /* Search the free list for a fit */
  if ((bp = find_fit(asize)) != NULL) {
    //printf("Fit found\n");
    place(bp, asize);
    return bp;
  }
  //printf("No fit found\n");
  /* No fit found. Get more memory and place the block */
  extendsize = MAX(asize,CHUNKSIZE);
  if ((bp = extend_heap(extendsize/WSIZE)) == NULL)
    return NULL;
  place(bp, asize);
  return bp;
}

/*
 * free
 */
void mm_free (void *ptr) {

if(ptr == 0) return;


  size_t size = GET_SIZE(HDRP(ptr));
  if (heap_listp == 0){
    mm_init();
  }

  // If block is already freed
  if (!GET_ALLOC(HDRP(ptr)))
  {
    return;
  }

  // Sets header and footer
  PUT(HDRP(ptr), PACK(size, 0));
  PUT(FTRP(ptr), PACK(size, 0));
  coalesce(ptr);
  mm_checkheap(0);
}

/*
 * realloc - you may want to look at mm-naive.c
 */
void *mm_realloc(void *oldptr, size_t size) {
  size_t oldsize;
  void *newptr;

  /* If size == 0 then this is just free, and we return NULL. */
  if(size == 0) {
    mm_free(oldptr);
    return 0;
  }

  /* If oldptr is NULL, then this is just malloc. */
  if(oldptr == NULL) {
    return mm_malloc(size);
  }

  newptr = mm_malloc(size);

  /* If realloc() fails the original block is left untouched  */
  if(!newptr) {
    return 0;
  }

  /* Copy the old data. */
  oldsize = GET_SIZE(HDRP(oldptr));
  if(size < oldsize) oldsize = size;
  memcpy(newptr, oldptr, oldsize);

  /* Free the old block. */
  mm_free(oldptr);

  return newptr;
}

/*
 * Return whether the pointer is in the heap.
 * May be useful for debugging.
 */
/*static int in_heap(const void *p) {
    return p < mem_heap_hi() && p >= mem_heap_lo();
    }*/

/*
 * Return whether the pointer is aligned.
 * May be useful for debugging.
 */
/*static int aligned(const void *p) {
    return (size_t)ALIGN(p) == (size_t)p;
    }*/

/*
 * mm_checkheap
 */

void mm_checkheap(int verbose) {
  char *bp = heap_listp;

  if (verbose)
    printf("Heap (%p):\n", heap_listp);

  if ((GET_SIZE(HDRP(heap_listp)) != 24) || !GET_ALLOC(HDRP(heap_listp)))
    //printf("Bad prologue header\n");
  checkblock(heap_listp);

  for (bp = heap_listp; GET_SIZE(HDRP(bp)) > 0; bp = NEXT_BLKP(bp))
    {
      if (verbose)
	printblock(bp);
      checkblock(bp);
    }

  if (verbose)
    printblock(bp);
  if ((GET_SIZE(HDRP(bp)) != 0) || !(GET_ALLOC(HDRP(bp))))
    {
    //printf("Bad epilogue header\n");
    }
}
	/* Remaining functions are all helpers */

/*
 * extend_heap - Extend heap with free block and return its block pointer
 */
/* $begin mmextendheap */
static void *extend_heap(size_t words)
{

  char *bp;
  size_t size;
  void *return_ptr;

  /* Allocate an even number of words to maintain alignment */
  size = (words % 2) ? (words+1) * WSIZE : words * WSIZE;
  if (size < CHUNKSIZE)
    size = CHUNKSIZE;
  if ((long)(bp = mem_sbrk(size)) < 0)
    return NULL;

  /* Initialize free block header/footer and the epilogue header */
  PUT(HDRP(bp), PACK(size, 0));         /* free block header */
  PUT(FTRP(bp), PACK(size, 0));         /* free block footer */
  PUT(HDRP(NEXT_BLKP(bp)), PACK(0, 1)); /* new epilogue header */

  /* Coalesce if the previous block was free */
  //printf("List before expansion\n");
  //print_freelist();
  return_ptr = coalesce(bp);
  //printf("EXPANDED HEAP\n");
  mm_checkheap(0);
  return return_ptr;
}
/* $end mmextendheap */

/*
 * place - Place block of asize bytes at start of free block bp
 *         and split if remainder would be at least minimum block size
 */
/* $begin mmplace */
/* $begin mmplace-proto */
static void place(void *bp, size_t asize)
  /* $end mmplace-proto */
{
  size_t csize = GET_SIZE(HDRP(bp));
  //printf("BLOCK TO BE REMOVED IN PLACE\n");
  //printblock(bp);
  remove_fb(bp);

  if ((csize - asize) >= ((2*DSIZE) + OVERHEAD)) {
    PUT(HDRP(bp), PACK(asize, 1));
    PUT(FTRP(bp), PACK(asize, 1));
    bp = NEXT_BLKP(bp);
    PUT(HDRP(bp), PACK(csize-asize, 0));
    PUT(FTRP(bp), PACK(csize-asize, 0));
    // Now to move the list pointers
   // printblock(bp);
    coalesce(bp);
  }
  else {
    PUT(HDRP(bp), PACK(csize, 1));
    PUT(FTRP(bp), PACK(csize, 1));
  }
}
/* $end mmplace */

/*
 * find_fit - Find a fit for a block with asize bytes
 */
static void *find_fit(size_t asize)
{
  /* first fit search */
  void *bp;

  for (bp = free_list; (bp != NULL) && !GET_ALLOC(HDRP(bp)); bp = NEXT_FBLKP(bp)) {
   // printf("Searching free list\n");
   // print_freelist();
    if (asize <= GET_SIZE(HDRP(bp))) {
      return bp;
    }
  }
  //printf("no fit\n");
  return NULL; /* no fit */
}

/*
 * coalesce - boundary tag coalescing. Return ptr to coalesced block
 */
static void *coalesce(void *bp)
{
  //printf("coalescing\n");
  size_t prev_alloc = GET_ALLOC(FTRP(PREV_BLKP(bp))) || PREV_BLKP(bp) == bp;
  size_t next_alloc = GET_ALLOC(HDRP(NEXT_BLKP(bp)));
  size_t size = GET_SIZE(HDRP(bp));
  char *nextbp = NEXT_BLKP(bp);
  char *prevbp = PREV_BLKP(bp);

  if (prev_alloc && next_alloc) {            /* Case 1 */
    insert_fb(bp);
    return bp;
  }

  else if (prev_alloc && !next_alloc) {      /* Case 2 */
    remove_fb(nextbp);
    size += GET_SIZE(HDRP(NEXT_BLKP(bp)));
    PUT(HDRP(bp), PACK(size, 0));
    PUT(FTRP(bp), PACK(size,0));
	// For the free list fixing
  }

  else if (!prev_alloc && next_alloc) {      /* Case 3 */
    remove_fb(prevbp);
    size += GET_SIZE(HDRP(PREV_BLKP(bp)));
    PUT(FTRP(bp), PACK(size, 0));
    PUT(HDRP(PREV_BLKP(bp)), PACK(size, 0));
    bp = PREV_BLKP(bp);
  }
  else {                                     /* Case 4 */
    remove_fb(prevbp);
    remove_fb(nextbp);
    size += GET_SIZE(HDRP(PREV_BLKP(bp))) +
      GET_SIZE(FTRP(NEXT_BLKP(bp)));
    PUT(HDRP(PREV_BLKP(bp)), PACK(size, 0));
    PUT(FTRP(NEXT_BLKP(bp)), PACK(size, 0));
    bp = PREV_BLKP(bp);
  }
  /* Add the newly combined block into the free list */
  //printf("list after coalesce:\n");
  //print_freelist();
  insert_fb(bp);
  return bp;
}

static void remove_fb(void *bp)
/* Given a free block, removes it from the list of free blocks */
{
  //printf("Removing\n");
  //printblock(bp);
	if (free_list == NULL)
		return;
	else if (bp == NULL)
		return;
	else
	{
	  if (free_list == bp)
	    {
	      if (NEXT_FBLKP(bp) == NULL)
    		{
    		  // Only item
    		  //printf("one item\n");
    		  free_list = NULL;
    		  return;
    		}
	      // First item
	      free_list = NEXT_FBLKP(bp);
	      PREV_FBLKP(NEXT_FBLKP(bp)) = NULL;
	      return;
	    }
      if (NEXT_FBLKP(bp) == NULL)
        {
          // Last item
          NEXT_FBLKP(PREV_FBLKP(bp)) = NULL;
          return;
        }
  	  else
        {
          // Middle item
          PREV_FBLKP(NEXT_FBLKP(bp)) = PREV_FBLKP(bp);
          NEXT_FBLKP(PREV_FBLKP(bp)) = NEXT_FBLKP(bp);
        }
	}
}
/*
static void print_freelist()
{
  char *cur_bp;
  //printf("FREE LIST SO FAR:\n");
  for (cur_bp = free_list; cur_bp != NULL; cur_bp = NEXT_FBLKP(cur_bp))
    {
      printblock(cur_bp);
    }
}
*/
static void insert_fb(void *bp)
/* Given a free block, inserts it into the list of free blocks
 * This may be altered later to implement address-ordered insertion */
{
  //printf("begin insert\n");
	// given block has no next or prev pointers yet
  //printblock(bp);
	if (free_list == NULL)
	  {
	    NEXT_FBLKP(bp) = NULL;
	    PREV_FBLKP(bp) = NULL;
	    free_list = bp;
	    //printf("FREE LIST STARTED\n");
	    //print_freelist();
	    return;
	  }
	//printf("Adding normally...\n");
	NEXT_FBLKP(bp) = free_list;	/* New block next pointer */
	PREV_FBLKP(bp) = NULL;	/* New block prev pointer */
  PREV_FBLKP(free_list) = bp;	/* Old block prev pointer */
	free_list = bp;
	//printf("end of insert\n");
	// Infinitely inserting??
}

static void printblock(void *bp)
{
  size_t hsize;// halloc, fsize, falloc;

  hsize = GET_SIZE(HDRP(bp));
  //halloc = GET_ALLOC(HDRP(bp));
  //fsize = GET_SIZE(FTRP(bp));
  //falloc = GET_ALLOC(FTRP(bp));

  if (hsize == 0) {
    printf("%p: EOL\n", bp);
    return;
  }
/*
    printf("%p: header: [%p:%c] footer: [%p:%c]\n", bp,
      hsize, (halloc ? 'a' : 'f'),
      fsize, (falloc ? 'a' : 'f')); 
      */
}

static void checkblock(void *bp)
{
  if ((size_t)bp % 8)
    printf("Error: %p is not doubleword aligned\n", bp);
  if (GET(HDRP(bp)) != GET(FTRP(bp)))
    printf("Error: header does not match footer\n");
}

void *mm_calloc (size_t nmemb, size_t size)
{
  void *ptr;
  if (heap_listp == 0){
    mm_init();
  }

  ptr = mm_malloc(nmemb*size);
  bzero(ptr, nmemb*size);


  return ptr;
}
