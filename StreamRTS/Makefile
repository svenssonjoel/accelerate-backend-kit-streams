



all: harness.o op1.o
	gcc harness.o -shared -o harness.so
	gcc op1.o -shared -o op1.so

harness.o: harness.c 
	gcc -c -std=c99 -fPIC harness.c -o harness.o

op1.o: op1.c 
	gcc -c -std=c99 -fPIC op1.c -o op1.o 
