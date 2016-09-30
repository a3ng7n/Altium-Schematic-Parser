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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <fcntl.h>
#include <unistd.h>

#include <timing.h>

#include "midismf.h"

#define SMF_MTHD        0x4D546864          // "MThd"
#define SMF_MTRK        0x4D54726B          // "MTrk"

#define MID_FORMAT_1_MULTI_CHANNEL  0x0000


/*
 * Functions to read a value from an 8-bit aligned address
 */

inline uint8_t read_8from8( const void * ptr )
{
    return *(uint8_t *)ptr;
}


inline uint16_t read_16from8( const void * ptr )
{
    return read_8from8( (uint8_t *)ptr+1 ) | ( (uint16_t)read_8from8( ptr ) << 8 );
}


inline uint32_t read_32from8(const void * ptr)
{
    return read_16from8( (uint8_t *)ptr+2 ) | ( (uint32_t)read_16from8( ptr ) << 16 );
}

 static uint32_t read_varlen(uint8_t **buf)
{
    uint8_t  b;
    uint32_t value = 0;

    do
    {
        b = **buf;
        (*buf)++;
        value = (value << 7) | (b & 0x7F);
    } while ((b & 0x80) == 0x80);

    return value;
}

void smf_play(smf_t *smf, midi_t *midi)
{
    for(midi_msg_t *msg = smf->track->msg; msg; msg = msg->next)
    {
        if (msg->eventid != SMF_META)
        {
            midi_txmsg(midi, msg);
            delay_us(msg->delta);
        }
    }
}

void smf_flushtracks(smf_t *smf)
{
    smf_track_t *track = smf->track;
    smf_track_t *nexttrack;

    while (track != NULL)
    {
        nexttrack = track->next;
        midi_flushmsgs(smf->track->msg);
        free(track);
        track = nexttrack;
    }
    smf->track = NULL;
}

int smf_parse(smf_t *smf, uint8_t *buf, uint32_t bufsize)
{
    uint32_t minsize;
    uint16_t division;
    int      trkcnt = 0;
    uint32_t tempo = 500000; // default tempo of a smf file
    uint32_t temp = 0;

    minsize = 7 * sizeof(uint16_t); // Size Midi header
    if ((smf == NULL) || (bufsize < minsize)) return 0;

    temp = read_32from8((void*)buf);
    if (temp == SMF_MTHD)
    {
        smf->length = read_32from8((void*)(buf + 4) );
        smf->format = read_16from8((void*)(buf + 8) );
        smf->ntrks  = read_16from8((void*)(buf + 10));
        division    = read_16from8((void*)(buf + 12));
        smf->ticks  = division >> 8;
        switch ((int8_t)smf->ticks)
        {
            case -24:
            {
                smf->division = SMF_DIVISION_TYPE_SMPTE24;
                break;
            }
            case -25:
            {
                smf->division = SMF_DIVISION_TYPE_SMPTE25;
                break;
            }
            case -29:
            {
                smf->division = SMF_DIVISION_TYPE_SMPTE30DROP;
                break;
            }
            case -30:
            {
                smf->division = SMF_DIVISION_TYPE_SMPTE30;
                break;
            }
            default:
            {
                smf->division = SMF_DIVISION_TYPE_PPQ;
                smf->ticks = division;
                break;
            }
        }
    }
    // unsupported format
    if (smf->format != MID_FORMAT_1_MULTI_CHANNEL) return 0;

    // skip over any extra header data
    buf = buf + 8 + smf->length;

    while (trkcnt++ < smf->ntrks)
    {
        smf_track_t *track, *prev_track = NULL;
        temp = read_32from8((void*)buf);
        buf += 4;
        if (temp == SMF_MTRK)
        {
            uint32_t tick;
            uint8_t status, running_status = 0;
            int at_end_of_track = 0;
            int msgcnt = 0;
            uint8_t *trackstart;
            midi_msg_t  *msg = NULL, *prev_msg = NULL;
            uint64_t usecs = 0;

            track = malloc(sizeof(smf_track_t));
            if (track == NULL)
            {
                smf_flushtracks(smf);
                return 0;
            }
            track->prev = prev_track;
            track->length = read_32from8((void*)(buf));
            buf += 4;
            trackstart = buf;
            while ((buf < trackstart + track->length) && !at_end_of_track)
            {
                tick = read_varlen(&buf);
                status = *buf++;
                uint8_t channel;
                uint8_t eventid;
                uint8_t data1;
                uint8_t data2;
                uint8_t valid_event = 1;

                if ((status & 0x80) == 0x00) // Is this a midi data byte
                {
                    status = running_status; // Yes, status = previous status
                    buf--;                   // go back one byte
                }
                else
                {
                    running_status = status; // No, record this status for next message
                }

                channel = (status & CHANNEL_MASK) + 1;
                eventid = status & EVENT_MASK;

                switch (eventid)
                {
                    case MIDI_NOTE_OFF:
                    case MIDI_NOTE_ON:
                    case MIDI_KEY_PRESSURE:
                    case MIDI_CONTROL_CHANGE:
                    {
                        data1 = *buf++;
                        data2 = *buf++;
                        msg = midi_createvoicemsg(prev_msg, usecs, eventid, channel, data1, data2);
                        break;
                    }
                    case MIDI_PROGRAM_CHANGE:
                    case MIDI_CHANNEL_PRESSURE:
                    {
                        data2 = *buf++;
                        msg = midi_createvoicemsg(prev_msg, usecs, eventid, channel, 0, data2);
                        break;
                    }
                    case MIDI_PITCH_BEND:
                    {
                        data2 = *buf++;
                        data2 = (data2 << 7) | *buf++;
                        msg = midi_createvoicemsg(prev_msg, usecs, eventid, channel, 0, data2);
                        break;
                    }
                    case 0xF0:
                    {
                        switch (status)
                        {
                            case MIDI_SYSTEMF0:
                            case MIDI_SYSTEMF7:
                            {
                                uint32_t length = read_varlen(&buf) + 1;

                                msg = midi_createsystemmsg(prev_msg, usecs, status, length, buf);
                                buf = buf + length;
                                break;
                            }
                            case SMF_META:
                            {
                                uint8_t number = *buf++;
                                uint32_t length = read_varlen(&buf);

                                if (number == MIDI_ENDOFTRACK)
                                    at_end_of_track = 1;
                                else
                                    msg = smf_createmetamsg(prev_msg, usecs, number, length, buf);
                                buf = buf + length;
                                if (msg && msg->event.meta.number == SMF_META_SET_TEMPO)
                                {
                                    tempo = (msg->event.meta.data[0] << 16) + (msg->event.meta.data[1] << 8) + msg->event.meta.data[2];
                                }
                                break;
                            }
                            default:
                                valid_event = 0;
                        }
                        break;
                    }
                    default:
                        valid_event = 0;
                }
                if (!prev_msg)
                    track->msg = msg; // add first message to track struct
                prev_msg = msg;
                if (valid_event && (msg == NULL))
                {
                    midi_flushmsgs(track->msg);
                    return 0;
                }
                usecs = smf_ticktotime(smf, tick, tempo);
            }
            if (prev_track)
                prev_track->next = track;
            else
                smf->track = track;  // add first track to smf struct
            if (track)
            {
                track->next = NULL;
                track->prev = prev_track;
                prev_track = track;
            }
            // Skip over any unrecognized chunks and extra data at the end of tracks
            buf = trackstart + track->length;
        }
    }
    return smf->ntrks;
}

/*
 * convert ticks to a time in usecs
 * for type SMF_DIVISION_TYPE_PPQ messages you need to provide
 * the current tempo in usecs per MIDI Quarter Note.
 */
uint64_t smf_ticktotime(smf_t *smf, uint32_t tick, uint32_t tempo)
{
    uint64_t usecs = 0;

    switch (smf->division)
    {
    case SMF_DIVISION_TYPE_PPQ:
        usecs = (uint64_t)((tick * (uint64_t)tempo) / smf->ticks);
        break;
    case SMF_DIVISION_TYPE_SMPTE24:
        usecs = (uint64_t)((tick * 1000000ULL) / (smf->ticks * 24));
        break;
    case SMF_DIVISION_TYPE_SMPTE25:
        usecs =  (uint64_t)((tick * 1000000ULL) / (smf->ticks * 25));
        break;
    case SMF_DIVISION_TYPE_SMPTE30DROP:
        usecs = (uint64_t)((tick * 100000000ULL) / (smf->ticks * 2997));
        break;
    case SMF_DIVISION_TYPE_SMPTE30:
        usecs = (uint64_t)((tick * 1000000ULL) / (smf->ticks * 30));
        break;
    default: ; // nothing
    }
    return usecs;
}

/*
 * convert time in usecs to midi ticks
 * for type SMF_DIVISION_TYPE_PPQ messages you need to provide
 * the current tempo in usecs per MIDI Quarter Note.
 */
uint32_t smf_timetotick(smf_t *smf, uint64_t usecs, uint32_t tempo)
{
    uint32_t tick = 500000; // default tempo of a smf file;

    switch (smf->division)
    {
    case SMF_DIVISION_TYPE_PPQ:
        tick = (uint32_t)((usecs * smf->ticks) / tempo);
        break;
    case SMF_DIVISION_TYPE_SMPTE24:
        tick = (uint32_t)((usecs * smf->ticks * 24) / 1000000ULL);
        break;
    case SMF_DIVISION_TYPE_SMPTE25:
        tick = (uint32_t)((usecs * smf->ticks * 25) / 1000000ULL);
        break;
    case SMF_DIVISION_TYPE_SMPTE30DROP:
        tick = (uint32_t)((usecs * smf->ticks * 2997) / 100000000ULL);
        break;
    case SMF_DIVISION_TYPE_SMPTE30:
        tick = (uint32_t)((usecs * smf->ticks * 30) / 1000000ULL);
        break;
    default: ; // nothing
    }
    return tick;
}


midi_msg_t *smf_createmetamsg(midi_msg_t * prev_msg, uint64_t delta, uint8_t number, uint32_t length, void *data)
{
    meta_t *meta = (meta_t *)data;
    midi_msg_t *msg = malloc(sizeof(midi_msg_t));

   if (prev_msg)
    {
        prev_msg->next = msg;
        prev_msg->delta = delta;
    }
    if (msg)
    {
        msg->next = NULL;
        msg->prev = prev_msg;
        msg->eventid = SMF_META;
        msg->channel = 0;  // not relevant
        msg->event.meta.number = number;
        msg->event.meta.length = length;
        msg->event.meta.data = malloc(length);
        memcpy(msg->event.meta.data, data, length);
    }
    return msg;
}

