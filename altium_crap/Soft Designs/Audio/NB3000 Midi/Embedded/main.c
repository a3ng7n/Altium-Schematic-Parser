/*****************************************************************************\
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*  IN PACKAGE:         Midi example
|*
|*  COPYRIGHT:          Copyright (c) 2009, Altium
|*
|*  DESCRIPTION:        Provides example for use of Midi ports applied on the
|*                      NB3000 Starterkit.
 */

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>

#include <timing.h>

#include "devices.h"
#include "midismf.h"                                                    // Routines to support Standard Midi Files
#include "midi.h"                                                       // used to drive the midi ports

static void init( void );
static void rxmidimsg(uint8_t eventid, uint8_t channel, uint8_t dbyte1, uint8_t dbyte2);

midi_t             *midi;
int                noe = 0; // received note off events;

// Standard Midi File 'sound.mid' is linked at compiletime
// see miscellaneous additional linker options of software project
extern __no_sdata uint8_t _lc_ub_sound_mid;
extern __no_sdata uint8_t _lc_ue_sound_mid;


/**
 * @brief Example of manipulating data transceived from/to the Midi ports
 */
int main(int argc, char * argv[])
{
    int         rec_cmd = 0;
    smf_t       smf;
    uint32_t    mid_size = &_lc_ue_sound_mid - &_lc_ub_sound_mid + 1;
    int         ntracks;

    // Init devices
    init();
    // Register user function that will be called when a midi message is received
    midi_regrxmsg(midi, &rxmidimsg);

    // Parsing Standard Midi File present in memory
    printf("\nParse Standard Midi file, play it on midi OUT\n");
    printf("and capture it on midi IN\n");
    ntracks = smf_parse(&smf, &_lc_ub_sound_mid, mid_size);
    if (!ntracks)
    {
        printf("Error parsing Standard Midi File\n");
        return -1;
    } else
    {
        printf("\nThere're %d track(s) in the parsed midi file\n", ntracks);
    }

    delay_ms(1000);
    midi_capture(midi, 1);
    smf_play(&smf, midi);
    smf_flushtracks(&smf);

    // Unregister user function
    midi_regrxmsg(midi, NULL);
    printf("\nReceived %d Note Off events on Midi IN\n", noe);

    printf("\nNow we alter the notes of the received messages\n");
    printf("and play back the edited midi messages on midi OUT\n");
    for(midi_msg_t *msg = midi_getfirstmsg(midi); msg; msg = msg->next)
    {
        if (msg->eventid == MIDI_NOTE_ON)
            msg->event.note_on.note += 12;
        else if(msg->eventid == MIDI_NOTE_OFF)
            msg->event.note_off.note += 12;
    }
    delay_ms(1000);
    midi_capture(midi, 0);
    midi_play(midi);

    midi_flushmsgs(midi_getfirstmsg(midi));
    puts("Done");
}


void rxmidimsg(uint8_t eventid, uint8_t channel, uint8_t dbyte1, uint8_t dbyte2)
{
    if (eventid == MIDI_NOTE_OFF)
       noe++;
}


/**********************************************************************
|*
|*  FUNCTION    : init
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Initialize hardware and drivers
 */
static void init( void )
{
    puts("Midi service example, " __FILE__ " compiled " __DATE__ ", " __TIME__ );
    puts("This example assumes Midi OUT is connected to Midi IN.");

    // Initialize Midi
    printf("OK\nInitializing midi... ");
    if (midi = midi_open(MIDI_1), !midi)
    {
        puts("Failed");
        abort();
    }
    puts("OK\nInit Ready.");
}

