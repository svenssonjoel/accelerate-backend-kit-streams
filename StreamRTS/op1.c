
#include <stdio.h> 

void op1 (void (*cont)(int), int a){

  printf("Op1: %d\n", a);

  cont(a); 
}
