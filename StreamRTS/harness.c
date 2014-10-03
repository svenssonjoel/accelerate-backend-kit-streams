
#include <stdio.h> 
#include <stdbool.h>
#include <memory.h>
#include <malloc.h>  

// Will lead to double indirection... 
// Linked list of nodes.. 
struct OPNODE { 
  void (*op)(struct OPNODE*, int);   // what to do now
  struct OPNODE *cont;               // what to do next
}; 


/* struct OPNODE_STATEFUL {  */
/*   State *MyState; */
/*   void (*op)(struct OPNODE*, int);   // what to do now */
/*   struct OPNODE *cont;               // what to do next */
/* };  */




#define runOp(opNode,a) (opNode)->op((opNode),a)   



struct OPNODE *mkNode( void (*op)(struct OPNODE*, int), 
		       struct OPNODE* cont) { 
  
  struct OPNODE *this = malloc(sizeof(struct OPNODE));
  
  this->op = op; 
  this->cont = cont;
   
  return this;
} 
	

// a new operator
void op1(struct OPNODE* this, int a) { 
  
  printf("Op1: %d\n",a); 

  runOp(this->cont,a);   
} 

void op2(struct OPNODE* this, int a) { 
  
  printf("Op2: %d\n",(a-100000)); 

  runOp(this->cont,a);   
} 

void op3(struct OPNODE* this, int a) { 
  
  printf("*** Op3: %d\n",a); 

  runOp(this->cont,a);   
} 

	
void terminator(struct OPNODE* this, int a) { 
  return; 
}  

// make these things robust when it comes to memory allocation/deallocation. 
struct OPNODE *termnode() {

  struct OPNODE *this=malloc(sizeof(struct OPNODE)); 
  this->op = &terminator; 
  this->cont = 0; //nullptr
  
  return this; 
}

		
// A source 
int in() {
  static int i = 0; 

  return i++;
}


// a sink 
void out(struct OPNODE*this, int a){
 
  printf("Sink: %d\n",a);
  
  runOp(this->cont,a);
}

int run = 1; 

void halt() {

  run = 0;
}

// void (*cont)(int) ;

void repoint(struct OPNODE* op, struct OPNODE* cont) {
  struct OPNODE *tmp = op->cont;
  op->cont = cont;
  cont->cont = tmp;
} 

// This may cause problems, as it destroys op
void replace(struct OPNODE* op, struct OPNODE *new_op) { 
  op->op=new_op->op;
  // keep cont? or replace with new_op's ?
  op->cont=new_op->cont;

} 

// the main loop 
int harness(struct OPNODE* op){ 
  
  int a; 

  while (run) {
    a = in(); 
    
    runOp(op,a);
    
  }
}
