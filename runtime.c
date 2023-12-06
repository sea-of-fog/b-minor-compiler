#include<stdio.h>
#include<stdbool.h>

void print_int(int n){
	printf("%ld", n);
	return;
}

void print_bool(bool b){
	printf("%s", b ? "true" : "false");
	return;
}
