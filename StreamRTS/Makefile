



all: harness.so op1.so

harness.so: harness.c 
	gcc -shared -std=c99 -fPIC harness.c -o harness.so

op1.so: op1.c 
	gcc -shared -std=c99 -fPIC op1.c -o op1.so 

clean: 
	rm harness.so
	rm op1.so
