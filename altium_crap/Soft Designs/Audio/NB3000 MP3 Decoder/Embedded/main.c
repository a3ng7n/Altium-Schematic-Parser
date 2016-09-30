/*****************************************************************************\
|*
|*  IN PACKAGE:         Software Platform Builder
|*
|*  COPYRIGHT:          Copyright (c) 2010, Altium
|*
|*  DESCRIPTION:        Shows how to use the MP3 Decoder driver
|*
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <fs.h>
#include <unistd.h>
#include <sys/stat.h>

// Application Stack interface
#include "swplatform.h"

#include "form1.h"

#define DIR_SEP       "/"
#define FAILED()      { printf( "*** Failed ***\n" ); }
#define INPUT_SIZE    (8 * 1024)

#define ERROR_LIMIT 3

sdcard_t *drv_sdcard;

static int audio_samplerate;

static uint16_t *mp3_output_pos;
static int mp3_input_bufnr;
static int mp3_input_count;
static int mp3_output_count;
static int mp3_error_count;

// these buffers must be allocated in external memory
uint32_t mp3_input_buf[2][INPUT_SIZE / 4];
uint32_t mp3_output_buf[576];

static int mp3_open(const char *filename);
static int mp3_skip_id3(int infile);
static bool mp3_process(int infile);
static bool mp3_errorreport(void);
static void init( void );


typedef enum
{
    STATE_DETECT,
    STATE_INIT,
    STATE_MOUNT,
    STATE_IDLE,
    STATE_PLAYING,
    STATE_RESET
} state_t;


/**********************************************************************
|*
|*  FUNCTION    : main
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : main program
 */

void main( void )
{
    int handle = 0;
    state_t state = STATE_RESET;
    int partition = 0;

    init();

    agui_show_form(AGUI_HANDLE(form1));

    for (;;)
    {
        gui_play = false; // set by GUI if 'play' pressed
        gui_stop = false; // set by GUI if 'stop' pressed
        agui_service(agui);

        // guard for card removal
        if ((state != STATE_DETECT) && !sdcard_detect(drv_sdcard))
        {
            state = STATE_RESET;
        }

        switch (state)
        {
        case STATE_RESET:
            unmount("/sdcard", MOUNT_FLAG_FORCE);

            listbox_clear(&form1_dirs);
            listbox_clear(&form1_files);
            obj_set_enabled(AGUI_HANDLE(form1_dirs), false);
            obj_set_enabled(AGUI_HANDLE(form1_files), false);
            obj_set_enabled(AGUI_HANDLE(form1_play), false);
            obj_set_enabled(AGUI_HANDLE(form1_stop), false);

            gui_info("Please insert SD card...");
            state = STATE_DETECT;
            break;

        case STATE_DETECT:
            if (sdcard_detect(drv_sdcard))
            {
                gui_info("Initializing card...");
                state = STATE_INIT;
            }
            break;

        case STATE_INIT:
            if (sdcard_device_init(drv_sdcard) == 0)
            {
                partition = 1;
                state = STATE_MOUNT;
            }
            break;

        case STATE_MOUNT:
            if (mount("/dev/BLOCKIO", "/sdcard", "fatfs", partition, MOUNT_FLAG_RDONLY) == 0)
            {
                dirlisting("/sdcard");
                listbox_sort(&form1_dirs, false);
                obj_set_enabled(AGUI_HANDLE(form1_dirs), true);
                obj_set_enabled(AGUI_HANDLE(form1_files), true);
                obj_set_enabled(AGUI_HANDLE(form1_play), false);
                obj_set_enabled(AGUI_HANDLE(form1_stop), false);

                gui_info("");
                state = STATE_IDLE;
            }
            else if (partition < 4)
            {
                ++partition;
            }
            else if (partition == 4)
            {
                partition = 0;
            }
            else
            {
                state = STATE_DETECT;
            }
            break;

        case STATE_IDLE:
            if (gui_play)
            {
                handle = mp3_open(gui_dir_file);
                if (!handle) break;

                obj_set_enabled(AGUI_HANDLE(form1_dirs), false);
                obj_set_enabled(AGUI_HANDLE(form1_files), false);
                obj_set_enabled(AGUI_HANDLE(form1_play), false);
                obj_set_enabled(AGUI_HANDLE(form1_stop), true);

                gui_info("Starting play...");
                state = STATE_PLAYING;
            }
            break;

        case STATE_PLAYING:
            if (gui_stop | !mp3_process(handle))
            {
                close(handle);
                obj_set_enabled(AGUI_HANDLE(form1_dirs), true);
                obj_set_enabled(AGUI_HANDLE(form1_files), true);
                obj_set_enabled(AGUI_HANDLE(form1_play), true);
                obj_set_enabled(AGUI_HANDLE(form1_stop), false);

                state = STATE_IDLE;
            }
            break;

        default:
            /* never used */
            break;

        }
    }
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
    puts( "MP3 decoder example, " __FILE__ " compiled " __DATE__ ", " __TIME__ );

    mp3dec_set_outputbuffer(drv_mp3dec, mp3_output_buf);

    drv_sdcard = sdcard_open(DRV_SDCARD);

    audio_set_volume(audio, 255);
}


/**********************************************************************
|*
|*  FUNCTION    : mp3_open
|*
|*  PARAMETERS  : filename = filename (inculding path) of MP3 to play
|*
|*  RETURNS     : filehandle
|*
|*  DESCRIPTION : Prepare to play an MP3 file
*/

static int mp3_open(const char *filename)
{
    int infile;

    printf("playing '%s'\n", filename);

    infile = open(filename, O_RDONLY);

    mp3_error_count = 0;
    mp3_input_bufnr = 0;
    mp3_input_count = 0;
    mp3_output_count = 0;
    audio_samplerate = 0;

    mp3_skip_id3(infile);
    mp3dec_decode(drv_mp3dec, NULL, 0);

    return infile;
}


/**********************************************************************
|*
|*  FUNCTION    : mp3_skip_id3
|*
|*  PARAMETERS  : filehandle of file being played
|*
|*  RETURNS     : fileposition or -1 if error
|*
|*  DESCRIPTION : skip ID3 tag if found at begin of file
*/
static int mp3_skip_id3(int infile)
{
    char buf[10], *b = buf;
    int tagsize = 0;

    // read the possile ID3 header
    if (read(infile, buf, 10) != 10) goto rewind;

    // ID3 marker
    if (*b++ != 'I') goto rewind;
    if (*b++ != 'D') goto rewind;
    if (*b++ != '3') goto rewind;

    // ignore version & flags
    b += 3;

    // read 28 bit integer
    for (int i = 0; i < 4; ++i)
    {
        if (*b & 0x80) goto rewind;
        tagsize = (tagsize << 7) + (int) *b++;
    }

    printf("found ID3 tag, skipping %i bytes\n", 10 + tagsize);

    return lseek(infile, tagsize, SEEK_CUR);

rewind:
    return lseek(infile, 0, SEEK_SET);

}

/**********************************************************************
|*
|*  FUNCTION    : mp3_process
|*
|*  PARAMETERS  : filehandle of file being played
|*
|*  RETURNS     : true while playing, false meaning EOF and decoding is ready
|*
|*  DESCRIPTION : Call repeatedly to play the MP3 file
*/
static bool mp3_process(int infile)
{
    uint32_t status = mp3dec_get_status(drv_mp3dec);

    if (status & MP3DEC_STATUS_HEADERREADY)
    {
        if (mp3_errorreport())
        {
            if (++mp3_error_count > ERROR_LIMIT) return false;
        }
    }

    if ((mp3_output_count == 0) && (status & MP3DEC_STATUS_WRITEREADY))
    {
        if (mp3_error_count)
        {
            // very crude error handling: discard this frame, tell decoder we want another
            // this avoids 'garbage files' to create audio by accident, while minimizing the effect
            // on legitimate files with only a few broken frames distributed over the file
            mp3dec_decode_continue(drv_mp3dec);
            --mp3_error_count;
        }
        else
        {
            // new decoded frame is ready
            int frame_samplerate = mp3dec_get_samplerate(drv_mp3dec);

            if (frame_samplerate && (frame_samplerate != audio_samplerate))
            {
                char buf[64];
                audio_set_format(audio, frame_samplerate, 2, 16);
                audio_samplerate = frame_samplerate;
                sprintf(buf, "Playing %i %s\n", frame_samplerate, (mp3dec_get_channelmode(drv_mp3dec) == 1) ? "mono" : "stereo");
                gui_info(buf);
            }

            mp3_output_pos = (uint16_t*) mp3_output_buf;
            mp3_output_count = 2 * 576;
        }
    }

    if (mp3_output_count > 0)

    {
        // play (part of) decoded data
        int played = audio_play(audio, mp3_output_pos, mp3_output_count);

        if (played < 0) return false; // audio error

        mp3_output_pos += played;

        if ((mp3_output_count -= played) == 0)
        {
            // tell decoder we want another frame
            mp3dec_decode_continue(drv_mp3dec);
        }
    }

    if ((mp3_input_count == 0) && (infile != 0))
    {
        // read new data into the inactive buffer
        mp3_input_count = read(infile, mp3_input_buf[mp3_input_bufnr], INPUT_SIZE);
        if (mp3_input_count == 0) mp3_input_count = -1;
    }

    if ((mp3_input_count > 0) && (status & MP3DEC_STATUS_READEMPTY))
    {
        // decoder ran out of input, give it the buffer we read
        mp3dec_set_inputdata(drv_mp3dec, mp3_input_buf[mp3_input_bufnr], mp3_input_count);

        // swap active/inactive buffer
        mp3_input_bufnr = 1 - mp3_input_bufnr;
        mp3_input_count = 0;
    }

    if ((mp3_input_count < 0) && (status & MP3DEC_STATUS_READEMPTY) && (mp3_output_count == 0))
    {
        // no more data to decode or play
        gui_info("");
        return false;
    }

    return true;
}


/**********************************************************************
|*
|*  FUNCTION    : mp3_errorreport
|*
|*  PARAMETERS  : None
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : Test MP3 headerword for unsupported flags
 */

static bool mp3_errorreport(void)
{
    uint32_t header = mp3dec_get_header(drv_mp3dec);
    bool errors = false;

    if ((header & 0x00060000) != 0x00020000) { errors = true; gui_info("Error: mp3-layer<>3\n"); }
    if ((header & 0x0000F000) == 0x00000000) { errors = true; gui_info("Error: free bitrate\n"); }
    if ((header & 0x00080000) != 0x00080000) { errors = true; gui_info("Error: low freq extension\n"); }
    if ((header & 0x00000003) != 0x00000000) { errors = true; gui_info("Error: emphasis\n"); }

    return errors;
}


/**********************************************************************
|*
|*  FUNCTION    : dirlisting
|*
|*  PARAMETERS  : path: path of the dir
|*
|*  RETURNS     : None
|*
|*  DESCRIPTION : List the contents of the dir into the GUI
 */

void dirlisting( const char *path )
{
    DIR             *dir;
    struct dirent   *dirent;
    struct stat     buf;
    char            path_to_file[PATH_MAX];

    chdir(path);
    dir = opendir(path);
    if ( dir != NULL )
    {
        printf( "Reading folder \"%s\"\n", path );
        listbox_clear(&form1_dirs);
        listbox_clear(&form1_files);
        while ( 1 )
        {
            dirent = readdir(dir);
            if (dirent == NULL)
            {
                break;
            }
            sprintf(path_to_file, "%s%s%s", path, DIR_SEP, dirent->d_name);
            if (stat(path_to_file, &buf) == 0)
            {
                if (S_ISDIR(buf.st_mode))
                {
                    if (strcmp(dirent->d_name, ".") == 0)
                    {
                        /* skip */
                    }
                    else
                    {
                        listbox_add(&form1_dirs, dirent->d_name, NULL);
                    }
                }
                else if (S_ISREG(buf.st_mode))
                {
                    listbox_add(&form1_files, dirent->d_name, NULL);
                }
                else
                {
                    printf("[UNK]\t%s\n", dirent->d_name);
                }
            }
            else
            {
                FAILED();
            }
        }

        if (closedir(dir) != 0)
        {
            FAILED();
        }
    }
}



