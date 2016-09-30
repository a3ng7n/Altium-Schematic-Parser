/******************************************************************************
 * FILE:        tetris_conf.h       
 * DESCRIPTION:
 *              tetris configuratiojn header file
 *****************************************************************************/
#ifndef _TETRIS_CONF_H
#define _TETRIS_CONF_H

#include <signal.h>

/* gravity hit : first / delta intervals in nscs */
#define SIGGFIRST_NSCS          500000000L
#define SIGGDELTA_NSCS          40000000L

/* number of completed lines to enter new phase */
#define NO_LINES_PHASE          6

/* width and height of the board -in cell units-
 * (H_BOARD+2)*(W_BOARD+2) board ( 2 is for frame) */
#define H_BOARD                 14
#define W_BOARD                 21
#define H_TOTAL                 H_BOARD +2
#define W_TOTAL                 W_BOARD +2

/* thread priorities */
#define INPUT_THREAD_PRIORITY           5
#define LOGGER_THREAD_PRIORITY          15
#define TETRIS_THREAD_PRIORITY          10
#define UPDATE_INFO_THREAD_PRIORITY     8

/* brightness levels */
#define BRIGHT_MAX              0xFF
#define BRIGHT_MIN              0x10

#endif
