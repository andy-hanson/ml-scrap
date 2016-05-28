// http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html#sec398

#include <stdio.h>

// external add: int -> int -> int = "add";;
int add(int a, int b) {
	int real_a = a >> 1;
	int real_b =  b >> 1;
	return ((real_a + real_b) << 1) | 1;
}

typedef unsigned char byte;

void interpret(unsigned char* start) {
	void* table[4] = {NULL, &&inst1, &&inst2, &&inst3};
	#define DISPATCH(n) start += n; goto *(table[*start]);
	DISPATCH(0);
	for (;;) {
		inst1:
			printf("first %d\n", *(start+1));
			DISPATCH(2);
		inst2:
			printf("second\n");
			DISPATCH(1);
		inst3:
			break;
	}
}

void poop(void) {
	unsigned char test[] = {0x1, 0x5, 0x1, 0x2, 0x2, 0x3};
	interpret(test);
	return;
}
