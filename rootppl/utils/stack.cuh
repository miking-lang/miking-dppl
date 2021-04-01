const int STACK_LIMIT = 2048;

struct pStack_t {
    int stackPointer = 0;
    // bblockArgs_t args[STACK_LIMIT];
    floating_t args[STACK_LIMIT];

    DEV void push(floating_t element) {
        if(stackPointer >= STACK_LIMIT || stackPointer < 0)
            printf("Illegal stack push with sp=%d\n", stackPointer);
        args[stackPointer] = element;
        stackPointer++;
    }

    DEV floating_t pop() {
        stackPointer--;
        if(stackPointer < 0)
            printf("SP < 0!\n");
        return args[stackPointer];
    }

    DEV floating_t peek() {
        if(stackPointer-1 < 0)
            printf("SP < 0!\n");
        return args[stackPointer - 1];
    }
};