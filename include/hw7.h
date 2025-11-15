#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

#define MAX_LINE_LEN 128

#ifndef __MATRIX_SF
#define __MATRIX_SF

typedef struct {
    char name;
    unsigned int num_rows;
    unsigned int num_cols;
    int values[]; 
} matrix_sf;

typedef struct bst_sf {
    matrix_sf *mat;
    struct bst_sf *left_child;
    struct bst_sf *right_child;
} bst_sf;

/**
 * @brief Given a pointer to the bst_sf struct root, which could be NULL, insert the provided matrix mat into the BST without making a copy of mat. The function must ensure that the sorted property of the BST is maintained. The function creates a new BST if the root is NULL.
 * @return a pointer to the root of the updated (or new) BST. 
 */
bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root); 
/**
 * @brief Given a pointer to the bst_sf struct root, which could be NULL, locate and return a pointer to a matrix named name, if it exists.
 * @return a pointer to a matrix or NULL
 */
matrix_sf* find_bst_sf(char name, bst_sf *root); 
/**
 * @brief Given a pointer to the bst_sf struct root, which could be NULL, free all the nodes of the tree. Also, free all the matrix_sf structs pointed to by the BST nodes. 
 */
void free_bst_sf(bst_sf *root); 
/**
 * @brief Perform the matrix addition mat1+mat2 and return the sum.
 */
matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2); 
/**
 * @brief Perform the matrix multiplication mat1*mat2 and return the product. 
 */
matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2); 
/**
 * @brief Return the transpose of mat. 
 */
matrix_sf* transpose_mat_sf(const matrix_sf *mat); 
/**
 * @brief Parse a string (expr) containing a valid definition of a new matrix and return a pointer to a correctly initialized matrix_sf struct.
 */
matrix_sf* create_matrix_sf(char name, const char *expr); 
/**
 * @brief Given the name of a file containing a script, execute the contents of the file and return a pointer to the final, named matrix created on the last line of the script.
 */
matrix_sf* execute_script_sf(char *filename); 
/**
 * @brief Evaluate expr and store the resulting matrix in a new matrix called name. 
 * @return a pointer to the new matrix
 */
matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root); 
/**
 * @brief Given an infix expression infix, convert it to its equivalent postfix expression.
 * @return the newly allocated string containing the postfix expression. 
 */
char* infix2postfix_sf(char *infix); 

// This is a utility function you may use if you want. See hw7.c.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]);
// Utility function used in testing. Don't mess with it.
void print_matrix_sf(matrix_sf *mat);

#endif // __MATRIX_SF
