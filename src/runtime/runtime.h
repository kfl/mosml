/* runtime.h */

#ifdef macintosh

/* 23Nov93  e */
/* 16Mar94  e */

void init_timers(void);
void beg_runtime(int);
void acc_runtime(int);

#define beg_gc_time() beg_runtime(1)
#define end_gc_time() acc_runtime(1)

#define beg_mf_time() beg_runtime(2)
#define end_mf_time() acc_runtime(2)

#else

void beg_gc_time();
void end_gc_time();

struct mosml_timeval {
	long	tv_sec;		/* seconds */
	long	tv_usec;	/* microseconds */
};

extern struct mosml_timeval gc_time;

#endif
