#include<stdio.h>
#include<stdbool.h>

void print_int(int n){
	printf("%d\n", n);
	return;
}

void print_bool(bool b){
	printf("%s\n", b ? "true" : "false");
	return;
}
