/*
 * DO NOT INCLUDE 'timer.h' IN YOUR PROJECT
 * instead of that include 'timer0.h' and/or 'timer1.h'
 */


/*
 * macros
 */
#define CONCAT_LOW(a,b)     tmr##a##_##b
#define ADDPREF_LOW(a,b)    CONCAT_LOW(a,b)
#define CONCAT_UP(a,b)      TMR##a##_##b
#define ADDPREF_UP(a,b)     CONCAT_UP(a,b)


/*
 * defines
 */
#define getclock        ADDPREF_LOW(TMR_NR,getclock)
#define system_time     ADDPREF_LOW(TMR_NR,system_time)
#define settimeout      ADDPREF_LOW(TMR_NR,settimeout)
#define expired         ADDPREF_LOW(TMR_NR,expired)
#define tick_down       ADDPREF_LOW(TMR_NR,tick_down)
#define reload_ticks    ADDPREF_LOW(TMR_NR,reload_ticks)
#define delay           ADDPREF_LOW(TMR_NR,delay)
#define interrupt       ADDPREF_LOW(TMR_NR,interrupt)
#define idle            ADDPREF_LOW(TMR_NR,idle)
#define atomic          ADDPREF_LOW(TMR_NR,atomic)
#define adj_sys_time    ADDPREF_LOW(TMR_NR,adj_sys_time)
#define init            ADDPREF_LOW(TMR_NR,init)
#define update_allowed  ADDPREF_LOW(TMR_NR,update_allowed)
#define TMR_TIMER       ADDPREF_UP(TMR_NR,TMR_TIMER)
#define TIMER_SIZE      ADDPREF_UP(TMR_NR,TIMER_SIZE)


/*
 * global vars
 */


/*
 * Timer type defines
 * pay attention to use signed arithmetic
 */
#if TIMER_SIZE == 4
typedef long TMR_TIMER;
#elif TIMER_SIZE == 2
typedef int TMR_TIMER;
#else
#error set TIMER_SIZE to 2 or 4
#endif

/*
 * function prototypes, inline functions
 */
TMR_TIMER getclock( void );
void delay( TMR_TIMER ticks );


/* return the time at which the timeout expires */
inline TMR_TIMER settimeout( TMR_TIMER ticks )
{
    return getclock() + ticks;
}


/* return TRUE if expired */
inline char expired( TMR_TIMER expire_time )
{
    return ( getclock() - expire_time ) >= 0 ? 1 : 0;
}


