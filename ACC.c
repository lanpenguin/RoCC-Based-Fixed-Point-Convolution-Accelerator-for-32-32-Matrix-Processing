#include "rocc.h"
#include <stdio.h>

#define N 32
#define K 5
#define PAD 2
#define GP 7
#define FP 8
#define TO_FIXED(x) ((unsigned long)((x) << FP))

unsigned long input[N][N];
unsigned long kernel[K][K];

unsigned long packedInput[N][N][GP];
unsigned long packedKernel[GP];
//unsigned long output[0];
unsigned long output_math[N][N];
unsigned long output_acc[N][N];

static inline unsigned long rdcycle()
{
    unsigned long cycles;
    asm volatile ("rdcycle %0" : "=r"(cycles));
    return cycles;
}

//static inline unsigned long packed_mac(unsigned long packedA, unsigned long packedB)
//{
    //unsigned long result;
    //ROCC_INSTRUCTION_DSS(0, result, packedA, packedB, 0);
    //return result;
//}
static unsigned long pack4_16(
    unsigned long x0,
    unsigned long x1,
    unsigned long x2,
    unsigned long x3
)
{
    unsigned long packed = 0;
    packed |= (x0 & 0xFFFFUL);
    packed |= (x1 & 0xFFFFUL) << 16;
    packed |= (x2 & 0xFFFFUL) << 32;
    packed |= (x3 & 0xFFFFUL) << 48;
    return packed;
}

static inline void set_input(unsigned long addr)
{
    unsigned long cmd = 0;
    ROCC_INSTRUCTION_SS(0, addr, cmd, 0);
}

static inline void set_kernel(unsigned long addr)
{
    unsigned long cmd = 1;
    ROCC_INSTRUCTION_SS(0, addr, cmd, 0);
}

static inline void set_output(unsigned long addr)
{
    unsigned long cmd = 2;
    ROCC_INSTRUCTION_SS(0, addr, cmd, 0);
}

static inline void start_acc(void)
{
    unsigned long zero = 0;
    unsigned long cmd = 3;
    ROCC_INSTRUCTION_SS(0, zero, cmd, 0);
}

static inline unsigned long read_status(void)
{
    unsigned long status;
    unsigned long zero = 0;
    unsigned long cmd = 4;
    ROCC_INSTRUCTION_DSS(0, status, zero, cmd, 0);
    return status;
}

//static inline unsigned long read_result(void)
//{
    //unsigned long result;
    //unsigned long zero = 0;
    //unsigned long cmd = 4;
    //ROCC_INSTRUCTION_DSS(0, result, zero, cmd, 0);
    //return result;
//}

void conv_math(void)
{
    int i, j, ki, kj;
    
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
        
            unsigned long acc = 0;
            
            for (ki = 0; ki < K; ki++) {
                for (kj = 0; kj < K; kj++) {
                
                    int ii = i + ki - PAD;
                    int jj = j + kj - PAD;
                    
                    unsigned long x;
                    
                    if (ii >= 0 && ii < N && jj >= 0 && jj < N) {
                        x = input[ii][jj];
                    } else {
                        x = 0;
                    }
                    
                    acc += (x * kernel[ki][kj]) >> FP;
                }
            }
              
            output_math[i][j] = acc;
        }
    }
}

//unsigned long software_output00(void)
//{
    //int ki, kj;
    //unsigned long acc = 0;
    
    //for (ki = 0; ki < K; ki++) {
        //for (kj = 0; kj < K; kj++) {
            //int ii = ki - PAD;
            //int jj = kj - PAD;
            //unsigned long x = 0;
            
            //if (ii >= 0 && ii < N && jj >= 0 && jj < N) {
                //x = input[ii][jj];
            //} //else {
                //x = 0;
            //}
            //acc += x * kernel[ki][kj];
        //}
    //}
    //return acc;    
//}

unsigned long get_input_by_idx(int row, int col, int idx)
{
    int ki = idx / K;
    int kj = idx % K;
    int ii = row + ki - PAD;
    int jj = col + kj - PAD;
    if (ii >= 0 && ii < N && jj >= 0 && jj < N) {
        return input[ii][jj];
    } else {
        return 0;
    }
}    

unsigned long get_kernel_by_idx(int idx)
{
    int ki = idx / K;
    int kj = idx % K;
    return kernel[ki][kj];
    //int ii = ki - PAD;
    //int jj = kj - PAD;
    //if (ii >= 0 && ii < N && jj >= 0 && jj < N) {
        //return input[ii][jj];
    //} else {
        //return 0;
    //}
}

void prepare_packed_kernel(void)
{
    int g;
    for (g = 0; g < GP; g++) {
        int base = g * 4;
        unsigned long b0 = 0;
        unsigned long b1 = 0;
        unsigned long b2 = 0;
        unsigned long b3 = 0;
        
        if (base + 0 < 25) b0 = get_kernel_by_idx(base + 0);
        if (base + 1 < 25) b1 = get_kernel_by_idx(base + 1);
        if (base + 2 < 25) b2 = get_kernel_by_idx(base + 2);
        if (base + 3 < 25) b3 = get_kernel_by_idx(base + 3);
        packedKernel[g] = pack4_16(b0, b1, b2, b3);
    }
            
}

void prepare_packed_input(void)
{
    int row, col, g;
    for (row = 0; row < N; row++) {
        for (col = 0; col < N; col++) {
            for (g = 0; g < GP; g++) {            
                int base = g * 4;
                unsigned long a0 = 0;
                unsigned long a1 = 0;
                unsigned long a2 = 0;
                unsigned long a3 = 0;
        
                if (base + 0 < 25) a0 = get_input_by_idx(row, col, base + 0);
                if (base + 1 < 25) a1 = get_input_by_idx(row, col, base + 1);
                if (base + 2 < 25) a2 = get_input_by_idx(row, col, base + 2);
                if (base + 3 < 25) a3 = get_input_by_idx(row, col, base + 3);
                packedInput[row][col][g] = pack4_16(a0, a1, a2, a3);
            }
        }
    }            
}
         
void conv_acc(void)
{
    unsigned long status;
    unsigned long timeout = 0;
    
    //printf("Set input address\n");
    set_input((unsigned long)packedInput);
    //printf("Set kernel address\n");
    set_kernel((unsigned long)packedKernel);
    //printf("Set output address\n");
    set_output((unsigned long)output_acc);
    
    //printf("Before start\n");
    start_acc();
    //printf("After start\n");
    
    do {
        status = read_status();
        
        
        if ((timeout % 1000000UL) == 0) {
            printf("status = %lu\n", status);
        }   
        timeout++;    
        
        if (timeout > 200000000UL) {
            printf("Error: accelerator timeout\n");
            printf("final status = %lu\n", status);
            return;
        }
    } while ((status & 0x1) == 0);
    
    if (status & 0x2) {
        printf("Error: accelerator error flag set\n");
    }
    
    asm volatile ("fence rw, rw" ::: "memory");
}


int main(void)
{
    int i, j;
    //unsigned long packedA;
    //unsigned long packedB;
    //unsigned long status;
    //unsigned long result;
    //unsigned long expected;
    //unsigned long timeout = 0;
    unsigned long start_math, end_math;
    unsigned long start_acc, end_acc;
    unsigned long cycle_math, cycle_acc;
    
    //unsigned long a0 = 1;
    //unsigned long a1 = 2;
    //unsigned long a2 = 3;
    //unsigned long a3 = 4;
    //unsigned long b0 = 10;
    //unsigned long b1 = 20;
    //unsigned long b2 = 30;
    //unsigned long b3 = 40;
    
    //packedInput[0] = pack4_16(a0, a1, a2, a3);
    //packedKernel[0] = pack4_16(b0, b1, b2, b3);
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            input[i][j] = TO_FIXED(i +j + 1);
            output_math[i][j] = 0;
            output_acc[i][j] = 0;
        }
    }
    for (i = 0; i < K; i++) {
        for (j = 0; j < K; j++) {
            kernel[i][j] = TO_FIXED(1);
        }
    }    
    //output[0] = 0;
    //expected  = software_output00();
    
    //printf("start\n");
    //printf("expected = %lu\n", expected);
    prepare_packed_kernel();
    prepare_packed_input();
    //set_input((unsigned long)packedInput);
    //set_kernel((unsigned long)packedKernel);
    //set_output((unsigned long)output);
    //start_acc();
    start_math = rdcycle();
    conv_math();
    end_math = rdcycle();
    //printf("After conv_math\n");
    
    //printf("Before conv_acc\n");
    start_acc = rdcycle();
    conv_acc();
    end_acc = rdcycle();
    cycle_math = end_math - start_math;
    cycle_acc = end_acc - start_acc;    
    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
        
            if (output_math[i][j] != output_acc[i][j]) {
                printf("Mismatch at [%d][%d]\n", i, j);
                printf("Math result = %lu\n", output_math[i][j]);
                printf("CPU result = %lu\n", output_acc[i][j]);
                return 1;
            }
        }
    }
    
    printf("Complete!\n");
    printf("All result match!\n");
    printf("Software cycles = %lu\n", cycle_math);
    printf("Hardware cycles = %lu\n", cycle_acc);
    
    if (cycle_acc != 0) {
        //printf("Speedup x100 = %lu\n", (cycle_math *100)/cycle_acc);
        //printf("Speedup = %.2f\n", (double)cycle_math / (double)cycle_acc);
        printf("Speedup = %lu.%02lu\n", cycle_math / cycle_acc, ((cycle_math % cycle_acc) * 100) / cycle_acc);
    }
    
    return 0;
}
    
    
