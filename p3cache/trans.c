/*
 * trans.c - Matrix transpose B = A^T
 *
 * Each transpose function must have a prototype of the form:
 * void trans(int M, int N, int A[N][M], int B[M][N]);
 *
 * A transpose function is evaluated by counting the number of misses
 * on a 1KB direct mapped cache with a block size of 32 bytes.
 */
#include <stdio.h>
#include "cachelab.h"

#define ROW_SIZE1 8
#define COL_SIZE1 8

#define ROW_SIZE2 8
#define COL_SIZE2 4

#define ROW_SIZE3 8
#define COL_SIZE3 4

int is_transpose(int M, int N, int A[N][M], int B[M][N]);



/*
 * You can define additional transpose functions below. We've defined
 * a simple one below to help you get started.
 */

/*
 * trans - customized the first one so it'll solve the first problem
 */
char trans_desc[] = "Blocking cache out, then finding diagonals (prob1)";
void trans(int M, int N, int A[N][M], int B[M][N])
{
	//Answer for prob 1 so M/N = 32
  int row, col, i, j, k, same;
  int same_flag = 0;
  // Outer 2 loops are for block location
  for (row=0;row < (N/ROW_SIZE1); row++)
    for (col=0;col < (M/COL_SIZE1); col++)
      // Inner 2 loops are for index
      for (i=(row*ROW_SIZE1);i < ((row+1)*ROW_SIZE1);i++)
	{
	  for (j=(col*COL_SIZE1);j < ((col+1)*COL_SIZE1);j++)
	  {
	    // We need to check if it's a diagonal
	    if (i == j)
	      {
		same = A[i][j];
		k = i;
		same_flag = 1;
	      }
	    else
	      {
		B[j][i] = A[i][j];
	      }
	    
	  }
	if (same_flag)
	  B[k][k] = same;
	same_flag = 0;
	}
}

/*
 * trans_uneven - Attempt at solving 32x64
 */
char trans_uneven_desc[] = "Transpose the 32x64";
void trans_uneven(int M, int N, int A[N][M], int B[M][N])
{
  int row, col, i, j;
  int temp, k;
  int temp_flag = 0;
  for (row=0;row < (N/ROW_SIZE3);row++)
    for (col=0;col < (M/COL_SIZE3);col++)
      for (i=(ROW_SIZE3*row);i < ((row+1)*ROW_SIZE3);i++)
	{
	for (j=(COL_SIZE3*col);j < ((col+1)*COL_SIZE3);j++)
	  {
	    // Keep checking for diags
	    if (i == j)
	      {
		temp = A[i][j];
		k = i;
		temp_flag = 1;
	      }
	    else
	      {
		B[j][i] = A[i][j];
	      }
	  }
	if (temp_flag)
	  B[k][k] = temp;
	temp_flag = 0;
	}
}

/*
 * trans2 - Attempt at solving 64x64 problem
 */
char trans2_desc[] = "Transpose Secondary Answer";
void trans2(int M, int N, int A[N][M], int B[M][N])
{
  int row, col, i, j;
  int temp, k;
  int temp_flag = 0;
  for (row=0;row < (N/ROW_SIZE2);row++)
    for (col=0;col < (M/COL_SIZE2);col++)
      for (i=(ROW_SIZE2*row);i < ((row+1)*ROW_SIZE2);i++)
	{
	for (j=(COL_SIZE2*col);j < ((col+1)*COL_SIZE2);j++)
	  {
	    if (i == j)
	      {
		temp = A[i][j];
		k = i;
		temp_flag = 1;
	      }
	    else
	      {
		B[j][i] = A[i][j];
	      }
	  }
	if (temp_flag)
	  B[k][k] = temp;
	temp_flag = 0;
	}
}

/*
 * trans_final - Attempt at solving 61x67 problem
 */
char trans_final_desc[] = "61x67 Answer (Using 18x18 blocks)";
void trans_final(int M, int N, int A[N][M], int B[M][N])
{
	int i, j, row, col;
	for (row=0;row < N;row += 18)
		for (col=0;col < M;col += 18)
			for (i=row;i < (row+18);i++)
			{
				for (j=col;j < (col+18);j++)
				{
					if ((i > 66) || (j > 60))
					{
						continue;
					}
					else
					{
						B[j][i] = A[i][j];
					}
				}
			}
}

/*
 * transpose_submit - This is the solution transpose function that you
 *     will be graded on for Part B of the assignment. Do not change
 *     the description string "Transpose submission", as the driver
 *     searches for that string to identify the transpose function to
 *     be graded.
 */
char transpose_submit_desc[] = "Transpose submission";
void transpose_submit(int M, int N, int A[N][M], int B[M][N])
{
    if ((M == 32) && (N == 32))
      trans(M,N,A,B);
    else if ((M == 32) && (N == 64))
      trans_uneven(M,N,A,B);
    else if ((M == 64) && (N == 64))
      trans2(M,N,A,B);
    else if ((M == 61) && (N == 67))
      trans_final(M,N,A,B);
}

/*
 * registerFunctions - This function registers your transpose
 *     functions with the driver.  At runtime, the driver will
 *     evaluate each of the registered functions and summarize their
 *     performance. This is a handy way to experiment with different
 *     transpose strategies.
 */
void registerFunctions()
{
    /* Register your solution function */
    registerTransFunction(transpose_submit, transpose_submit_desc);

    /* Register any additional transpose functions */
    registerTransFunction(trans, trans_desc);

    /* Another */
    registerTransFunction(trans_uneven, trans_uneven_desc);

    /* Secondary help function */
    registerTransFunction(trans2, trans2_desc);

    /* 61x67 Solution */
    registerTransFunction(trans_final, trans_final_desc);
}



/*
 * is_transpose - This helper function checks if B is the transpose of
 *     A. You can check the correctness of your transpose by calling
 *     it before returning from the transpose function.
 */
int is_transpose(int M, int N, int A[N][M], int B[M][N])
{
    int i, j;

    for (i = 0; i < N; i++) {
        for (j = 0; j < M; ++j) {
            if (A[i][j] != B[j][i]) {
                return 0;
            }
        }
    }
    return 1;
}

