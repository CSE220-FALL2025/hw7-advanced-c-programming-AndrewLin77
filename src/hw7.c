#include "hw7.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>

bst_sf* insert_bst_sf(matrix_sf *mat, bst_sf *root) {
    if(root == NULL) {
        bst_sf *newnode = malloc(sizeof(bst_sf));
        newnode->mat = mat;
        newnode->left_child = NULL;
        newnode->right_child = NULL;
        return newnode;
    }

    if(mat->name < root->mat->name) {
        root->left_child = insert_bst_sf(mat, root->left_child);
    } else if(mat->name > root->mat->name) {
        root->right_child = insert_bst_sf(mat, root->right_child);
    } else {
        free(root->mat);
        root->mat = mat;
    }
    return root;
}

matrix_sf* find_bst_sf(char name, bst_sf *root) {
    if(root == NULL) {
        return NULL;
    }

    if(name == root->mat->name) {
        return root->mat;
    } else if(name < root->mat->name) {
        return find_bst_sf(name, root->left_child);
    } else {
        return find_bst_sf(name, root->right_child);
    }
}

void free_bst_sf(bst_sf *root) {
    if(root == NULL) {
        return;
    }

    free_bst_sf(root->left_child);

    free_bst_sf(root->right_child);

    if(root->mat != NULL) {
        free(root->mat);
    }
    
    free(root);
}

matrix_sf* add_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if(mat1 == NULL || mat2 == NULL) {
        return NULL;
    }

    if(mat1->num_rows != mat2->num_rows || mat1->num_cols != mat2->num_cols) {
        return NULL;
    }

    matrix_sf *result = malloc(sizeof(matrix_sf) + mat1->num_rows * mat1->num_cols * sizeof(int));

    if(result == NULL) {
        return NULL;
    }

    result->name = '\0';
    result->num_rows = mat1->num_rows;
    result->num_cols = mat1->num_cols;

    for(unsigned int i = 0; i < (mat1->num_rows * mat1->num_cols); i++) {
        result->values[i] = mat1->values[i] + mat2->values[i];
    }

    return result;
}

matrix_sf* mult_mats_sf(const matrix_sf *mat1, const matrix_sf *mat2) {
    if(mat1 == NULL || mat2 == NULL) {
        return NULL;
    }

    if(mat1->num_cols != mat2->num_rows) {
        return NULL;
    }

    matrix_sf *result = malloc(sizeof(matrix_sf) + mat1->num_rows * mat2->num_cols * sizeof(int));

    if(result == NULL) {
        return NULL;
    }

    result->name = '\0';
    result->num_rows = mat1->num_rows;
    result->num_cols = mat2->num_cols;

    for(unsigned int i = 0; i < mat1->num_rows; i++) {
        for(unsigned int j = 0; j < mat2->num_cols; j++) {
            int sum = 0;
            for(unsigned int k = 0; k < mat1->num_cols; k++) {
                int idxA = i * mat1->num_cols + k;
                int idxB = k * mat2->num_cols + j;
                sum += mat1->values[idxA] * mat2->values[idxB];
            }
            result->values[i * mat2->num_cols + j] = sum;
        }
    }

    return result;
}

matrix_sf* transpose_mat_sf(const matrix_sf *mat) {
    if(mat == NULL) {
        return NULL;
    }
    
    matrix_sf *result = malloc(sizeof(matrix_sf) + mat->num_cols * mat->num_rows * sizeof(int));

    if(result == NULL) {
        return NULL;
    }

    result->name = '\0';
    result->num_rows = mat->num_cols;
    result->num_cols = mat->num_rows;

    for(unsigned int i = 0; i < mat->num_rows; i++) {
        for(unsigned int j = 0; j < mat->num_cols; j++) {
            unsigned int oldIdx = i * mat->num_cols + j;
            unsigned int newIdx = j * mat->num_rows + i;
            result->values[newIdx] = mat->values[oldIdx];
        }
    }

    return result;
}

matrix_sf* create_matrix_sf(char name, const char *expr) {
    const char *p = expr;
    unsigned int NR = strtoul(p, (char**)&p, 10);
    unsigned int NC = strtoul(p, (char**)&p, 10);

    while(*p && *p != '[') {
        p++;
    }

    if(*p == '[') {
        p++;
    }

    matrix_sf *m = malloc(sizeof(matrix_sf) + NR * NC * sizeof(int));
    if(!m) {
        return NULL;
    }

    m->name = name;
    m->num_rows = NR;
    m->num_cols = NC;

    unsigned int count = 0;

    while(count < NR * NC) {
        while(*p && !((*p == '-') || (*p >= '0' && *p <= '9'))) {
            p++;
        }
        m->values[count++] = strtol(p, (char**)&p, 10);
    }

    return m;
}

int precedence(char operator) {
    if(operator == '\'') {
        return 3;
    }
    if(operator == '*') {
        return 2;
    }
    if(operator == '+') {
        return 1;
    }
    return 0;
}

char* infix2postfix_sf(char *infix) {
    size_t n = strlen(infix);

    char *output = malloc(n * 2 + 1);
    int out_idx = 0;

    char *stack = malloc(n);
    int top = -1;

    for(size_t i = 0; i < n; i++) {
        char c = infix[i];

        if(isspace((unsigned char)c)) {
            continue;
        }

        if(isupper((unsigned char)c)) {
            output[out_idx++] = c;
            continue;
        }
        
        if(c == '(') {
            stack[++top] = c;
            continue;
        }

        if(c == ')') {
            while(top >= 0 && stack[top] != '(') {
                output[out_idx++] = stack[top--];
            }
            top--;
            continue;
        }

        if(c == '+' || c == '*' || c == '\'') {
            while(top >= 0 && precedence(stack[top]) >= precedence(c)) {
                output[out_idx++] = stack[top--];
            }
            stack[++top] = c;
        }
    }

    while(top >= 0) {
        output[out_idx++] = stack[top--];
    }

    output[out_idx] = '\0';

    free(stack);
    return output;
}

matrix_sf* evaluate_expr_sf(char name, char *expr, bst_sf *root) {
    if(expr == NULL) {
        return NULL;
    }

    char *postfix = infix2postfix_sf(expr);
    if(postfix == NULL) {
        return NULL;
    }

    size_t n = strlen(postfix);

    matrix_sf **stack = malloc(sizeof(matrix_sf*) * (n + 1));
    if(!stack) {
        free(postfix);
        return NULL;
    }

    int top = -1;
    for(size_t i = 0; i < n; i++) {
        char c = postfix[i];

        if(c == ' ') continue;

        if(isupper(c)) {
            matrix_sf *m = find_bst_sf(c, root);

            if(!m) {
                while(top >= 0) {
                    if(stack[top] && !isupper(stack[top]->name)) {
                        free(stack[top]);
                    }
                    top--;
                }
                free(stack);
                free(postfix);
                return NULL;
            }
            stack[++top] = m;
        } else if(c == '\'') {
            if(top < 0) {
                continue;
            }
            matrix_sf *A = stack[top--];
            matrix_sf *T = transpose_mat_sf(A);

            if(!T) {
                if(!isupper(A->name)) {
                    free(A);
                }
                while(top >= 0) {
                    if(stack[top] && !isupper(stack[top]->name)) {
                        free(stack[top]);
                    }
                    top--;
                }
                free(stack);
                free(postfix);
                return NULL;
            }

            T->name = '\0';
            stack[++top] = T;

            if(!isupper(A->name)) {
                free(A);
            }

        } else if(c == '*' || c == '+') {
            if(top < 1) {
                continue;
            }
            matrix_sf *B = stack[top--];
            matrix_sf *A = stack[top--];
            matrix_sf *C;

            if(c == '*') {
                C = mult_mats_sf(A, B);
            } else {
                C = add_mats_sf(A, B);
            }

            if(!C) {
                if(!isupper(A->name)) {
                    free(A);
                }
                if(!isupper(B->name)) {
                    free(B);
                }
                while(top >= 0) {
                    if(stack[top] && !isupper(stack[top]->name)) {
                        free(stack[top]);
                    }
                    top--;
                }
                free(stack);
                free(postfix);
                return NULL;
            }

            C->name = '\0';
            stack[++top] = C;

            if(!isupper(A->name)) {
                free(A);
            }
            if(!isupper(B->name)) {
                free(B);
            }
        } 
    }
    matrix_sf *result = stack[top];
    result->name = name;

    free(stack);
    free(postfix);
    
    return result;
}

matrix_sf *execute_script_sf(char *filename) {
   FILE *file = fopen(filename, "r");
    if(!file) {
        return NULL;
    }

    bst_sf *root = NULL;
    matrix_sf *last_matrix = NULL;
    char *line = NULL;
    size_t bufsize = 0;


    while(getline(&line, &bufsize, file) != -1) {
        char *p = line;

        while(*p && isspace((unsigned char)*p)) {
            p++;
        }

        if(!isupper((unsigned char)*p)) {
            continue;
        }

        char name = *p++;

        while(*p && isspace((unsigned char)*p)) {
            p++;
        }

        if(*p != '=') {
            continue;
        }

        p++;

        while(*p && isspace((unsigned char)*p)) {
            p++;
        }

        matrix_sf *new_matrix = NULL;

        if(isdigit((unsigned char)*p) || *p == '-') {
            new_matrix = create_matrix_sf(name, p);
        } else {
            new_matrix = evaluate_expr_sf(name, p, root);
        }

        if(new_matrix) {
            root = insert_bst_sf(new_matrix, root);
            last_matrix = new_matrix;
        }
    }

    free(line);
    fclose(file);

    if(!last_matrix) {
        free_bst_sf(root);
        return NULL;
    }
    matrix_sf *result = copy_matrix(last_matrix->num_rows,last_matrix->num_cols,last_matrix->values);
    free_bst_sf(root);

    return result;
}


// This is a utility function used during testing. Feel free to adapt the code to implement some of
// the assignment. Feel equally free to ignore it.
matrix_sf *copy_matrix(unsigned int num_rows, unsigned int num_cols, int values[]) {
    matrix_sf *m = malloc(sizeof(matrix_sf)+num_rows*num_cols*sizeof(int));
    m->name = '\0';
    m->num_rows = num_rows;
    m->num_cols = num_cols;
    memcpy(m->values, values, num_rows*num_cols*sizeof(int));
    return m;
}

// Don't touch this function. It's used by the testing framework.
// It's been left here in case it helps you debug and test your code.
void print_matrix_sf(matrix_sf *mat) {
    assert(mat != NULL);
    assert(mat->num_rows <= 1000);
    assert(mat->num_cols <= 1000);
    printf("%d %d ", mat->num_rows, mat->num_cols);
    for (unsigned int i = 0; i < mat->num_rows*mat->num_cols; i++) {
        printf("%d", mat->values[i]);
        if (i < mat->num_rows*mat->num_cols-1)
            printf(" ");
    }
    printf("\n");
}
