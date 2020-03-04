#ifdef BUILD_BOCMAN

#include <string.h>
#include <stdlib.h>

#include "pacman.h"
#include "osdepend.h"

#include "pacman_map.c"

//#define XOFF 34
#define XOFF 2
#define YOFF 14

// ghosts
#define OBJ_RANDY  0
#define OBJ_SEEKER 1
#define OBJ_MIMIC  2
#define OBJ_MIRROR 3
#define NUM_GHOSTS (OBJ_MIRROR+1)

// player should be low-priority sprite
#define OBJ_PLYR      NUM_GHOSTS
#define NUM_OBJS      (1+OBJ_PLYR)

// normally the sprite is the same as the object
// but b/c only sprite 7 has collision detection
// define the sprite for a hack
#define SPRITE_PLYR   7

#define PACMAN_TICK     5
#define PILL_TICK       48

/* type definitions */

typedef struct
{
    // these need to be int for now...
    // until the compiler bug is fixed
    int x;
    int y;
    unsigned char  o;
    unsigned char  dir;
    unsigned char  inputs;
    unsigned char  cell;

} OBJ_DATA;

/* global variables */

static volatile XDATA unsigned char tick = PACMAN_TICK;
static volatile XDATA unsigned char tick2 = 0;
static volatile XDATA unsigned char pill_tick = 0;

static volatile BIT freeze = 1;

static OBJ_DATA obj_data[] =
{
    // ghosts
    { 13, 15, 0, DIR_LEFT, 0, 0 },
    { 13, 15, 0, DIR_LEFT, 0, 0 },
    { 13, 15, 0, DIR_LEFT, 0, 0 },
    { 13, 15, 0, DIR_LEFT, 0, 0 },
    
    // player
    { 13, 24, 0, DIR_LEFT, 0, 0 }
};

// player inputs stored separately
static volatile unsigned char inputs = 0;

XDATA unsigned char board[32][28];
XDATA unsigned char score[4] = { 0, 0, 0, 0 };
XDATA unsigned char pellets;
XDATA unsigned char pills;
XDATA unsigned char ghost_value;

/* routines */

int pacman_printat(int x, int y, ROM char *str)
{
    unsigned char c;
    for (; *str; str++)
    {
        c = *str;
        if (*str >= '0' && *str <= ':')
            c = *str - '0' + 0x40;
        else
        if (*str >= 'A' && *str <= '[')
            c = *str - 'A' + 0x21;
        
        PUTTILE (c, x++, y);
    }
    
    return (x);
}

typedef struct
{
    char            chk_dx;
    char            chk_dy;
    char            dx;
    char            dy;
    unsigned char   new_o;
    
} MOVDAT;

static ROM MOVDAT movdat[] =
{
    // same/opposite direction

    // left 
    { -1,  0, -1,  0,  6 },
    {  0,  0,  0,  0,  0 },
    {  0,  0,  0,  0,  2 },
    {  0,  0,  0,  0,  4 },

    // right
    { +1,  0,  0,  0, 2 },
    {  0,  0,  0,  0, 4 },
    {  0,  0,  0,  0, 6 },
    {  0,  0, +1,  0, 0 },
    
    // up
    {  0, -1,  0, -1, 6 },
    {  0,  0,  0,  0, 0 },
    {  0,  0,  0,  0, 2 },
    {  0,  0,  0,  0, 4 },
    
    // down
    {  0, +1,  0,  0, 2 },
    {  0,  0,  0,  0, 4 },
    {  0,  0,  0,  0, 6 },
    {  0,  0,  0, +1, 0 },

    // 90 deg turns
    { -1,  0, -1,  0, 6 },      // left
    { +1,  0,  0,  0, 2 },      // right
    {  0, -1,  0, -1, 6 },      // up
    {  0, +1,  0,  0, 2 }       // down
};

#define LR_UD_MASK  (1<<2)
#define LU_MASK     (1<<0)

#define IS_TURN_90(a,b)  (((a ^ b) & LR_UD_MASK) != 0)
#define IS_LEFT_RIGHT(a) ((a & LR_UD_MASK) == 0)
#define IS_UP_DOWN(a)    ((a & LR_UD_MASK) != 0)
#define IS_LEFT_UP(a)    ((a & LU_MASK) == 0)
#define REVERSE(a)       (a ^ LU_MASK)

static ROM unsigned char plyr_sprite[][4] =
{
    { 0x84, 0x82, 0x80, 0x82 },      // left
    { 0x04, 0x02, 0x00, 0x02 },      // right
    { 0x84, 0x83, 0x81, 0x83 },      // up
    { 0x04, 0x03, 0x01, 0x03 }       // down
};

static ROM unsigned char ghost_normal_sprite[][4] =
{
    { 0x09, 0x0A, 0x09, 0x0A },      // left
    { 0x05, 0x06, 0x05, 0x06 },      // right
    { 0x0B, 0x0C, 0x0B, 0x0C },      // up
    { 0x07, 0x08, 0x07, 0x08 }       // down
};

static ROM unsigned char ghost_scared_sprite[][4] =
{
    { 0x0D, 0x0E, 0x0D, 0x0E },      // left
    { 0x0D, 0x0E, 0x0D, 0x0E },      // right
    { 0x0D, 0x0E, 0x0D, 0x0E },      // up
    { 0x0D, 0x0E, 0x0D, 0x0E }       // down
};

static ROM unsigned char (*ghost_sprite)[4] = ghost_normal_sprite;

static int chk_move (int n, unsigned char dir)
{
    OBJ_DATA *obj = &obj_data[n];
    
    int i = -1;

    if (dir)
    {
        if (IS_TURN_90(obj->dir, dir))
        {
            // 90 deg turns only at offset=0
            if (obj->o == 0)
                i = 16 + dir - DIR_LEFT;
        }
        else
            // same/opposite direction
            i = ((dir - DIR_LEFT) << 2) + (obj->o >> 1);
    }
    
    if (i != -1)
    {           
        unsigned char b = board[obj->y+movdat[i].chk_dy][obj->x+movdat[i].chk_dx];
        // ghosts can't leave the cage if we ate a pill
        if (b == 0xFF || (b == 0xFE && (dir != DIR_UP || pill_tick != 0)))
            i = -1;
    }

    return (i);
}

static int update_obj (unsigned char n, unsigned char input, ROM unsigned char table[][4])
{
    OBJ_DATA *obj = &obj_data[n];
    int i = -1;

    if (input != 0xFF)
    {
        if ((i = chk_move (n, input)) == -1)
        {
            input = obj->dir;
            i = chk_move (n, input);
        }
    
        if (i != -1)
        {
            if (obj->dir == input)
                obj->cell = (obj->cell + 1) & 0x03;
            else
                obj->cell = 1;
    
            obj->dir = input;
            obj->x += movdat[i].dx;
            obj->y += movdat[i].dy;
            obj->o = movdat[i].new_o;
        }
    }

    // quick hack for the collision detection on sprite 7
    if (n == OBJ_PLYR) n = SPRITE_PLYR;
    
    // set the sprite flip and cell
    SPRITE_N( n, table[obj->dir-DIR_LEFT][obj->cell]&0x3F);
    
    if (IS_LEFT_RIGHT (obj->dir))
    {
       SPRITE_FLIP_X (n, (table[obj->dir-DIR_LEFT][0] & 0x80 ? 1 : 0));
       PUTSPRITE( n, 
                  ((XOFF+obj->x)<<3)+obj->o-4, 
                  ((YOFF+obj->y)<<3)+12);
    }
    else
    {
        SPRITE_FLIP_Y (n, (table[obj->dir-DIR_LEFT][0] & 0x80 ? 1 : 0));
        PUTSPRITE(n, 
                  ((XOFF+obj->x)<<3)-4,
                  ((YOFF+obj->y)<<3)+obj->o+12);
    }

    return (i != -1);
}

void add_score (unsigned char hundreds, unsigned char tens)
{
    BIT leading = 0;

    score[3] += tens;
    if (score[3] > 9)
    {
       score[3] -= 10;
       hundreds++;
    }

    score[2] += hundreds;
    if (score[2] > 9)
    {
       score[2] -= 10;
       if (++score[1] > 9)
       {
          score[1] -= 10;
          ++score[0];
       }
    }

    if ((leading = (score[0] > 0)))
       PUTTILE (score[0]+0x40, XOFF+2, YOFF+1);
    if (leading || (leading = score[1] > 0))
       PUTTILE (score[1]+0x40, XOFF+3, YOFF+1);
    if (leading || (leading = score[2] > 0))
       PUTTILE (score[2]+0x40, XOFF+4, YOFF+1);
    PUTTILE (score[3]+0x40, XOFF+5, YOFF+1);
}

#ifndef MULTI_MCU
static void pacman_printbyte(int x, int y, ROM char *str, unsigned char value)
{
    unsigned char c;
    int x2 = pacman_printat(x, y, str);

    c = value >> 4;
    PUTTILE ((c>9 ? c+23 : c+0x40), x2++, y);
    c = value & 0x0f;
    PUTTILE ((c>9 ? c+23 : c+0x40), x2++, y);
}

static void show_debug (void)
{
    // some debug here
    pacman_printbyte (33, YOFF+ 3,  "PLAYER X ", obj_data[OBJ_PLYR].x);
    pacman_printbyte (40, YOFF+ 4,         "Y ", obj_data[OBJ_PLYR].y);
    pacman_printbyte (33, YOFF+ 6,  "RANDY  X ", obj_data[OBJ_RANDY].x);
    pacman_printbyte (40, YOFF+ 7,         "Y ", obj_data[OBJ_RANDY].y);
    pacman_printbyte (33, YOFF+ 8,  "SEEKER X ", obj_data[OBJ_SEEKER].x);
    pacman_printbyte (40, YOFF+ 9,         "Y ", obj_data[OBJ_SEEKER].y);
    pacman_printbyte (33, YOFF+10,  "MIMIC  X ", obj_data[OBJ_MIMIC].x);
    pacman_printbyte (40, YOFF+11,         "Y ", obj_data[OBJ_MIMIC].y);
    pacman_printbyte (33, YOFF+12,  "MIRROR X ", obj_data[OBJ_MIRROR].x);
    pacman_printbyte (40, YOFF+13,         "Y ", obj_data[OBJ_MIRROR].y);
    pacman_printbyte (34, YOFF+15,   "PELLETS ", pellets);
    pacman_printbyte (36, YOFF+16,     "PILLS ", pills);
    pacman_printbyte (32, YOFF+17, "PILL TICK ", pill_tick);
}
#endif

static BIT eatSndFlag = 0;
static XDATA char ateGhost = 0;
static XDATA char ghostEaten = 0;

void pacman_tick_handler (void)
{
    OBJ_DATA *obj;
    volatile int i;
    static XDATA int ic = 0;

    if (freeze)
        return;

    if (--tick != 0)
        return;

    tick = PACMAN_TICK;

    if (tick2 == 0)
    {
       for (i=0; i<16; i++)
       {
           static ROM char insertcoin[] = "INSERT COIN - 20c = 1 CREDIT... ";
           while (LCD_BUSY)
                 ;
           LCD_PUTCHAR (i, 1, insertcoin[(ic+i)&31]);
       }
       ic++;
    }

    if (ateGhost)
    {
       if (--ateGhost == 0)
       {
          add_score (ghost_value, 0);
          ghost_value <<= 1;
          // send him back to the cage
          obj_data[ghostEaten].x = 13;
          obj_data[ghostEaten].y = 15;
          update_obj (ghostEaten, 0xFF, ghost_sprite);
          *collide = 0;
       }
       else
          return;
    }
    else
       // check for collision
       if (IS_COLLISION(ghostEaten))
       {
          if (pill_tick == 0)
          {
             freeze = 1;
             return;
          }

          // ate a ghost here
          PLAY_SOUND (0x4AA, 0xDA1);
          ateGhost = 16;
          return;
       }

    if (tick2 == 0 && pill_tick)
    {
        if (--pill_tick < 16)
            ghost_sprite = (pill_tick & 1 ? ghost_scared_sprite : ghost_normal_sprite);
    }

    // wipe in the opposite direction
    for (i=0; i<NUM_GHOSTS; i++)
        WIPESPRITE (NUM_GHOSTS-1-i);
    WIPESPRITE (SPRITE_PLYR);

    obj = &obj_data[OBJ_PLYR];
    if (obj->o == 0)
    {
       if ((i = board[obj->y][obj->x]) != 0)
       {
          if (i == 1)
          {
             pellets--;
             add_score (0, 1);
          }
          else
          {
             pill_tick = PILL_TICK;
             ghost_sprite = ghost_scared_sprite;
             pills--;
             add_score (0, 5);
             // reset ghost score to 100 points
             ghost_value = 1;
          }

          if ((eatSndFlag = !eatSndFlag) != 0)
          {
             PLAY_SOUND (0x0000, 0x04AA);
          }
          board[obj->y][obj->x] = 0;
          PUTTILE (0x7F, XOFF+obj->x, YOFF+2+obj->y);
          update_obj (OBJ_PLYR, 0xFF, plyr_sprite);
       }
       else
       {
          // move the player
          update_obj (OBJ_PLYR, inputs, plyr_sprite);
       }
    }
    else
    {
       // move the player
       update_obj (OBJ_PLYR, inputs, plyr_sprite);
    }

    ++tick2;
    if (tick2 < 4 || (!pill_tick && tick2 < 5))
    {
#if 1
        // randy - turns 90 at junction if possible
        int r;
        obj = &obj_data[OBJ_RANDY];
        do
        {
            r = (rand() & 0x03) + DIR_LEFT;

        } while (!IS_TURN_90(r,obj->dir));
        if (chk_move (OBJ_RANDY, r) != -1)
            update_obj (OBJ_RANDY, r, ghost_sprite);
        else
        if (chk_move (OBJ_RANDY, obj->dir) != -1)
            update_obj (OBJ_RANDY, obj->dir, ghost_sprite);
        else
            update_obj (OBJ_RANDY, REVERSE(obj->dir), ghost_sprite);

        // seeker - moves towards player
        obj = &obj_data[OBJ_SEEKER];
        if (obj->y < obj_data[OBJ_PLYR].y && chk_move (OBJ_SEEKER, DIR_DOWN) != -1)
            //update_obj (OBJ_SEEKER, DIR_DOWN, ghost_sprite); /* bug */
            r = DIR_DOWN;
        else
        if (obj->y > obj_data[OBJ_PLYR].y && chk_move (OBJ_SEEKER, DIR_UP) != -1)
            //update_obj (OBJ_SEEKER, DIR_UP, ghost_sprite); /* bug */
            r = DIR_UP;
        else
        {
            if (obj->x < obj_data[OBJ_PLYR].x && chk_move (OBJ_SEEKER, DIR_RIGHT) != -1)
                r = DIR_RIGHT;
            else
            if (obj->x > obj_data[OBJ_PLYR].x && chk_move (OBJ_SEEKER, DIR_LEFT) != -1)
                r = DIR_LEFT;
            else
                r = (rand() & 0x03) + DIR_LEFT;
        }
        // if we ate a pill, run away from the player!
        if (pill_tick) r = REVERSE(r);
        update_obj (OBJ_SEEKER, r, ghost_sprite);
        
        // mimic - mimics player's movements
        obj = &obj_data[OBJ_MIMIC];
        if (chk_move (OBJ_MIMIC, obj_data[OBJ_PLYR].dir) != -1)
            r = obj_data[OBJ_PLYR].dir;
        else
            r = (rand() & 0x03) + DIR_LEFT;
       update_obj (OBJ_MIMIC, r, ghost_sprite);
    
        // mirror - mirrors player's movements
        obj = &obj_data[OBJ_MIRROR];
        if (chk_move (OBJ_MIRROR, REVERSE(obj_data[OBJ_PLYR].dir)) != -1)
            r = REVERSE(obj_data[OBJ_PLYR].dir);
        else
            r = (rand() & 0x03) + DIR_LEFT;
       update_obj (OBJ_MIRROR, r, ghost_sprite);
#else
        // for debug only           
        for (i=0; i<NUM_GHOSTS; i++)
        {
            int r = (rand() & 0x03) + DIR_LEFT;
            update_obj (i, r, ghost_sprite);
        }
#endif
    }
    else
    {
        for (i=0; i<NUM_GHOSTS; i++)
            update_obj (i, 0xFF, ghost_sprite);
    }
    if (tick2 == 5)
        tick2 = 0;

#ifndef MULTI_MCU
    // if the following line is commented-out
    // - the project fails to build
    //   cannot locate 2 sections : .stack_data, .stack
    show_debug ();
#endif
}

static ROM unsigned char cage_hack[] = 
{
    0xff, 0xff, 0xfe, 0xfe, 0xff, 0xff,
    0xff, 0x00, 0x00, 0x00, 0x00, 0xff,
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff
};

void pacman_init (void)
{
    int x, y, i;
    ROM unsigned char *pMap = map_data;

#ifdef MULTI_MCU
    pacman_printat (24, 3,       "ALTIUM L");
    pacman_printat (18, 4, "ALTERA CYCLONE");
    pacman_printat (20, 5,   "POWERED BY N");
#else                                    
    for (i=0; i<16; i++)
    {
        static ROM char lcdtitle[] = "     BOCMAN     ";
        while (LCD_BUSY)
              ;
        LCD_PUTCHAR (i, 0, lcdtitle[i]);
    }
    pacman_printat (XOFF, 3, "             NEXAR");
    pacman_printat (XOFF, 5, "    EMBEDDED SYSTEMS ON FPGAS");
    pacman_printat (XOFF, 7, "         ALTIUM LIMITED");
    pacman_printat (XOFF, 8, "MAKING ELECTRONICS DESIGN EASIER");
#endif
    pacman_printat (XOFF,11, "       EVALBOARD BOCMAN");

    // draw the text
    pacman_printat (XOFF+3, YOFF+0, "1UP");
    pacman_printat (XOFF+9, YOFF+0, "HIGH SCORE");
    pacman_printat (XOFF+5, YOFF+1, "00");
    pacman_printat (XOFF+12, YOFF+1, "26910");

    // init board
    for (y=0; y<(pellet_data[0]&63); y++)
       board[y&31][y>>5] = 0;
    y = 0;
    for (x=0; x<240; x++)
    {
        int z;
        for (z=y+1; z<y+(pellet_data[x]&63); z++)
            board[z&31][z>>5] = 0;
        board[z&31][z>>5] = 1;
        y = z;
    }
    for (++y; y<32*28; y++)
       board[y&31][y>>5] = 0;
    
    // power pills
    board[4][1] = 2;
    board[4][26] = 2;
    board[24][1] = 2;
    board[24][26] = 2;

    x = 0; y = 0;
    while (x < 14 && y < 32)
    {
        if (*pMap < 0x30)
        {
            for (i=0; i<*pMap-1; i++)
            {
                if (++y == 32)
                {
                    x++; y = 0;
                }
            }
        }
        else
        {
            if (*pMap < 0x7C)
            {
                board[y][x] = 0xff;
                board[y][27-x] = 0xff;
            }   
            PUTTILE (*pMap^0x01, XOFF+x, YOFF+2+y);
            PUTTILE (*pMap, XOFF+27-x, YOFF+2+y);
            if (++y == 32)
            {
                x++; y = 0;
            }
        }
        pMap++;
    }

    // door for ghosts to escape
    board[13][13] = 0xfe;
    board[13][14] = 0xfe;

    // hack the cage area
    i = 0;
    for (y=14; y<17; y++)
        for (x=11; x<17; x++)
            board[y][x] = cage_hack[i++];

    // draw board
    for (y=0; y<32; y++)
        for (x=0; x<28; x++)
            switch (board[y][x])
            {
                case 1 :
                    PUTTILE (0x4C, XOFF+x, YOFF+2+y);
                    break;
                case 2 :
                    PUTTILE (0x4D, XOFF+x, YOFF+2+y);
                    break;
                default :
                    break;
            }

    pellets = 240;
    pills = 4;

    // init & show the player sprite
    SPRITE_FLIP (SPRITE_PLYR, 1, 0);
    SPRITE_COLOUR (SPRITE_PLYR, 0, BRIGHT(COLOUR_YELLOW), 0);
    SPRITE_N (SPRITE_PLYR, 2);
    obj_data[OBJ_PLYR].cell = 1;
    PUTSPRITE (SPRITE_PLYR,
               ((XOFF+obj_data[OBJ_PLYR].x)<<3)-4, 
               ((YOFF+obj_data[OBJ_PLYR].y)<<3)+12);

    //for (i=0; i<NUM_SPRITES; i++)
    for (i=0; i<NUM_GHOSTS; i++)
    {
       static unsigned char ghost_colour[] = 
       {
          // randy    seeker          mimic        mirror
          COLOUR_RED, COLOUR_MAGENTA, COLOUR_CYAN, COLOUR_GREEN
       };
    
       // uncommenting the next line cause the build to fail
       //   cannot locate 2 sections : .stack_data, .stack
       //if (i > 0 && i <= NUM_GHOSTS)
       {
          // show the ghost sprites
          SPRITE_COLOUR (i, BRIGHT(COLOUR_BLUE), BRIGHT(ghost_colour[i]), COLOUR_WHITE);
          SPRITE_N (i, 5);
          PUTSPRITE (i,
                     ((XOFF+obj_data[i].x)<<3)-4, 
                     ((YOFF+obj_data[i].y)<<3)+12);
       }
    }
    
    pacman_printat (XOFF+9, YOFF+14, "PLAYER ONE");
    pacman_printat (XOFF+11, YOFF+20, "READY[");

    // reset the sprite collision latch
    *collide = 0;

    // init sound
    SOUND_DIR_UP;
}

int pacman_main (void)
{
    while (!lastKeyPressed)
       ;

    pacman_printat (XOFF+9, YOFF+14, "          ");
    pacman_printat (XOFF+11, YOFF+20, "      ");

    freeze = 0;
    while (1)
    {
       if (lastKeyPressed == KEY_ESC)
          break;

       if (lastKeyPressed == KEY_LEFT) inputs = DIR_LEFT;
       else if (lastKeyPressed == KEY_RIGHT) inputs = DIR_RIGHT;
       else if (lastKeyPressed == KEY_UP) inputs = DIR_UP;
       else if (lastKeyPressed == KEY_DOWN) inputs = DIR_DOWN;
       else inputs = 0;

        if (pellets == 0 && pills == 0)
            freeze = 1;

       if (freeze)
       {
          pacman_printat (XOFF+9, YOFF+20, "GAME  OVER");
          return (1);
       }
    }

    return (0);
}
#endif

