/* Functions for abi-test.sch: argument passing in registers and on the
 * stack, mixed integer and floating-point arguments, 64-bit values,
 * sign extension of results, and pointers into bytevectors. */

#include <stdint.h>
#include <string.h>
int  t_ints12(int a,int b,int c,int d,int e,int f,int g,int h,int i,int j,int k,int l)
{ return a+2*b+3*c+4*d+5*e+6*f+7*g+8*h+9*i+10*j+11*k+12*l; }
long long t_mixed(int a, double x, long long b, float y, int c, double z)
{ return a + (long long)(x*1000) + b + (long long)(y*100) + c + (long long)(z*10); }
double t_dbl10(double a,double b,double c,double d,double e,double f,double g,double h,double i,double j)
{ return a+2*b+3*c+4*d+5*e+6*f+7*g+8*h+9*i+10*j; }
float t_flt10(float a,float b,float c,float d,float e,float f,float g,float h,float i,float j)
{ return a+2*b+3*c+4*d+5*e+6*f+7*g+8*h+9*i+10*j; }
/* 9 ints then mixed stack: int, long long, int, float, double on the stack */
double t_stackmix(int a,int b,int c,int d,int e,int f,int g,int h,
                  double d1,double d2,double d3,double d4,double d5,double d6,double d7,double d8,
                  int s1, long long s2, int s3, float s4, double s5)
{ return a+b+c+d+e+f+g+h + d1+d2+d3+d4+d5+d6+d7+d8 + s1*1.0 + s2*10.0 + s3*100.0 + s4*1000.0 + s5*10000.0; }
int t_neg(void) { return -7; }
unsigned t_ubig(void) { return 0xFFFFFFF0u; }
long long t_llmin(void) { return INT64_MIN; }
unsigned long long t_ullmax(void) { return UINT64_MAX; }
unsigned long long t_ullid(unsigned long long x) { return x; }
long long t_llid(long long x) { return x; }
void t_fill(unsigned char *p, int n) { for (int i=0;i<n;i++) p[i]=(unsigned char)(i*3); }
int t_sum_bytes(const unsigned char *p, int n) { int s=0; for (int i=0;i<n;i++) s+=p[i]; return s; }
