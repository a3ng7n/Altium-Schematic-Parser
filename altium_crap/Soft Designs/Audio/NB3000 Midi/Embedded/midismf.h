/*****************************************************************************\
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*  IN PACKAGE:
|*
|*  COPYRIGHT:          Copyright (c) 2007, Altium
|*
|*  DESCRIPTION:
|*
 */

#ifndef _MIDISMF_H
#define _MIDISMF_H

#include "midi.h"

#define SMF_META 0XFF
#define SMF_META_SET_TEMPO 0X51

typedef struct smf_track_s smf_track_t;

struct smf_track_s
{
    uint32_t       length;    // track length
    midi_msg_t     *msg;      // first message/event in track
    smf_track_t    *prev;     // previous track in file
    smf_track_t    *next;     // next track in file
};

typedef enum
{
    SMF_DIVISION_TYPE_INVALID = -1,
    SMF_DIVISION_TYPE_PPQ,
    SMF_DIVISION_TYPE_SMPTE24,
    SMF_DIVISION_TYPE_SMPTE25,
    SMF_DIVISION_TYPE_SMPTE30DROP,
    SMF_DIVISION_TYPE_SMPTE30
} smf_divisiontype_t;

typedef struct
{
    uint32_t              length;    // Header length
    uint16_t              format;    // 0 = single multi-channel track, 1 = one or more simultaneous tracks, 2 = one or more sequentially independent single-track patterns
    uint16_t              ntrks;     // number of track chunks
    smf_divisiontype_t    division;  // division type
    uint16_t              ticks;     // ticks per frame for DIVISON_TYPE_SMPTE / ticks per quarter note for DIVISON_TYPE_PPQ
    smf_track_t           *track;    // first track in file
} smf_t;



extern void smf_flushtracks(smf_t *smf);
extern int smf_parse(smf_t *smf, uint8_t *buf, uint32_t bufsize);
extern void smf_play(smf_t *smf, midi_t *midi);
extern uint64_t smf_ticktotime(smf_t *smf, uint32_t tick, uint32_t tempo);
extern uint32_t smf_timetotick(smf_t *smf, uint64_t usecs, uint32_t tempo);
extern midi_msg_t *smf_createmetamsg(midi_msg_t * prev_msg, uint64_t delta, uint8_t number, uint32_t length, void *data);

#endif // _MIDISMF_H
