#include "rocc.h"
#include <stdio.h>

#define N 32
#define K 5
#define PAD 2

static inline void mac_write(unsigned long data_1, unsigned long data_2)
{
    ROCC_INSTRUCTION_SS(0, data_1, data_2, 0);
}

static inline unsigned long mac_compute(void)
{
    unsigned long value;
    ROCC_INSTRUCTION_DS(0, value, 0, 1);
    return value;
}

static inline void mac_reset(void)
{
    ROCC_INSTRUCTION(0, 2);
}

static inline unsigned long rdcycle()
{
    unsigned long cycles;
    asm volatile ("rdcycle %0" : "=r"(cycles));
    return cycles;
}

unsigned long input[N][N];

unsigned long kernel[K][K];

unsigned long output_acc[N][N];

unsigned long output_sw[N][N];

void conv_sw(void)
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

                    acc += x * kernel[ki][kj];
                }
            }

            output_sw[i][j] = acc;
        }
    }
}

void conv_acc(void)
{
    int i, j, ki, kj;

    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {

            mac_reset();

            for (ki = 0; ki < K; ki++) {
                for (kj = 0; kj < K; kj++) {

                    int ii = i + ki - PAD;
                    int jj = j + kj - PAD;

                    unsigned long x;
                    unsigned long k;

                    if (ii >= 0 && ii < N && jj >= 0 && jj < N) {
                        x = input[ii][jj];
                    } else {
                        x = 0;
                    }

                    k = kernel[ki][kj];

                    mac_write(x, k);
                }
            }

            output_acc[i][j] = mac_compute();
        }
    }
}


int main(void)
{
    int i, j;
    unsigned long start_sw, end_sw;
    unsigned long start_acc, end_acc;
    unsigned long sw_cycles, acc_cycles;

    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {
            input[i][j] = i + j + 1;
            output_sw[i][j] = 0;
            output_acc[i][j] = 0;
        }
    }

    for (i = 0; i < K; i++) {
        for (j = 0; j < K; j++) {
            kernel[i][j] = 1;
        }
    }

    start_sw = rdcycle();
    conv_sw();
    end_sw = rdcycle();

    start_acc = rdcycle();
    conv_acc();
    end_acc = rdcycle();

    sw_cycles = end_sw - start_sw;
    acc_cycles = end_acc - start_acc;

    for (i = 0; i < N; i++) {
        for (j = 0; j < N; j++) {

            if (output_sw[i][j] != output_acc[i][j]) {
                printf("Mismatch at [%d][%d]\n", i, j);
                printf("SW  = %lu\n", output_sw[i][j]);
                printf("ACC = %lu\n", output_acc[i][j]);
                return 1;
            }
        }
    }

    printf("Convolution complete!\n");
    printf("All results match!\n");

    printf("SW cycles  = %lu\n", sw_cycles);
    printf("ACC cycles = %lu\n", acc_cycles);

    if (acc_cycles != 0) {
        printf("Speedup = %lu\n", sw_cycles / acc_cycles);
    }

    printf("Sample outputs:\n");
    printf("output[0][0]   = %lu\n", output_acc[0][0]);
    printf("output[0][1]   = %lu\n", output_acc[0][1]);
    printf("output[1][1]   = %lu\n", output_acc[1][1]);
    printf("output[15][15] = %lu\n", output_acc[15][15]);
    printf("output[31][31] = %lu\n", output_acc[31][31]);

    printf("success\n");

    return 0;
}
