/*****************************************************************************\
|*
|* Filesystem Example
|*
|* Devices:
|* - CF card (WB_IDE and COMPACT_FLASH component)
|* - SD card (WB_SPI and SD_CARD component)
|* - S29 paralel flash (WB_SHARED_MEM_CTRL)
|*
|* Services
|* - Filesystem (fs_fat)
|*
|* Description:
|*      This example uses the fs_fat plugin to interact with FAT filesystems.
|*      Several features of the fs_fat API are demonstrated, including copying
|*      and moving files, creating a filesystem and listing filesystem
|*      contents.
|*
|*      The SD card is assumed to have no partition table and should contain
|*      JPEG images in its root directory.
|*
|*      There is an alternative embedded project, SSAS_Partition_Table that
|*      shows an example of creating a partition table. It uses the same FPGA
|*      project.
|*
\*****************************************************************************/

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include <fs.h>
#include <dirent.h>
#include <sys/stat.h>
#include "devices.h"

/* devices (note: keep in sync with SwPlatform file) */
#define DEV_SD_CARD         "/dev/BLOCKIO_1"
#define DEV_S29_FLASH       "/dev/BLOCKIO_2"

/* mount points */
#define MNT_SD_CARD         "/sdcard"
#define MNT_S29_FLASH       "/s29flash"

/* configuration: */

/* - source and destination */
#define SRC_DEV             DEV_SD_CARD
#define DST_DEV             DEV_S29_FLASH
#define SRC_MNT             MNT_SD_CARD
#define DST_MNT             MNT_S29_FLASH

/* - directories and patterns */
#define SRC_DIR             MNT_SD_CARD
#define CP_DIR              "CP_DIR"
#define CP_PATTERN          "JPG"
#define MV_PATTERN          "IMG1"
#define DIR_SEP             "/"
#define MV_DIR              "MV_DIR"

/* - directory creation */
#define MKDIR_CP            1
#define MKDIR_MV            1

/* - file system creation (Note: settings this to 1 will destroy the contents of DEST_DEV) */
#define MKFS                1

/* return codes */
#define FAILED( status )    { printf( "*** Failed ***\n" ); return status; }

enum
{
    E_OK,
    E_MOUNT,
    E_CHDIR,
    E_MKDIR,
    E_RMDIR,
    E_OPENDIR,
    E_OPEN,
    E_READ,
    E_WRITE,
    E_REMOVE,
    E_MKFS,
    E_RENAME,
    E_STAT,
    E_CLOSEDIR,
    E_GETCWD
};

static int dirlisting( const char *path );
static int copyfiles( const char *srcdir, const char *dstdir, const char *pattern );
static int movefiles( const char *srcdir, const char *dstdir, const char *pattern );

/*
 * Main program
 */
int main( void )
{
    int err, status;
    char curdir[PATH_MAX], *p;

    printf( "\n*** Filesystem Tests ***\n\n" );

#if MKFS
    printf( "+ Create destination filesystem: " );
    if (mkfs(DST_DEV, "fatfs", 0, "New FAT fs", false) < 0)
    {
        FAILED( E_MKFS );
    }
    printf( "Ok\n" );
#endif

    printf( "+ Mount source filesystem: " );

    /* Try to mount the first partition ... */
    err = mount(SRC_DEV, SRC_MNT, "fatfs", 1, MOUNT_FLAG_RDONLY);
    if (err != 0)
    {
        /* ... and if that fail try the entire disk */
        err = mount(SRC_DEV, SRC_MNT, "fatfs", 0, MOUNT_FLAG_RDONLY);
        if (err != 0)
        {
            FAILED( E_MOUNT );
        }
    }
    printf( "Ok\n" );

    printf( "+ Mount destination filesystem: " );

    /* Try to mount the first partition ... */
    err = mount(DST_DEV, DST_MNT, "fatfs", 1, MOUNT_FLAG_RDWR);
    if (err != 0)
    {
        /* ... and if that fail try the entire disk */
        err = mount(DST_DEV, DST_MNT, "fatfs", 0, MOUNT_FLAG_RDWR);
        if (err != 0)
        {
            FAILED( E_MOUNT );
        }
    }
    printf( "Ok\n" );

    printf( "+ List source directory:\n" );
    dirlisting( SRC_DIR );

#if MKDIR_CP
    printf( "+ Create destination directory: " );
    if (chdir(DST_MNT) != 0)
    {
        FAILED( E_CHDIR );
    }
    if ( mkdir( CP_DIR, S_IRWXU | S_IRWXG | S_IRWXO ) != 0 )
    {
        FAILED( E_MKDIR );
    }
    printf( "Ok\n" );

    printf( "+ List current working directory:\n");
    p = getcwd(curdir, sizeof(curdir));
    if (p == NULL)
    {
        FAILED( E_GETCWD );
    }
    dirlisting(p);

    printf( "+ List destination directory:\n" );
    dirlisting(CP_DIR);

    printf( "+ Copy files:" );
    status = copyfiles( SRC_DIR, CP_DIR, CP_PATTERN );
    if ( status == E_OK )
    {
        printf( "+ List destination directory:\n" );
        dirlisting( CP_DIR );
    }
    else
    {
        FAILED( status );
    }
#endif

#if MKDIR_MV
    printf( "+ Create 2nd destination directory: " );
    if ( mkdir( MV_DIR, S_IRWXU | S_IRWXG | S_IRWXO ) != 0 )
    {
        FAILED( E_MKDIR );
    }
    printf( "Ok\n" );
#endif

    printf( "+ Move files:" );
    status = movefiles( CP_DIR, MV_DIR, MV_PATTERN );
    if ( status == E_OK )
    {
        printf( "+ List 2nd destination directory:\n" );
        dirlisting( MV_DIR );
        printf( "+ List original destination directory:\n" );
        dirlisting( CP_DIR );
    }
    else
    {
        FAILED( status );
    }

    printf( "\n*** All Tests Passed ***\n" );

    return E_OK;
}

/*
 * File system operations
 */
#define BLOCKSIZE       512
#define MAXCOPIES       33

#ifndef MIN
# define MIN( a, b )    ( ( (a) <= (b) ) ? (a) : (b) )
#endif

/* Print a listing of directory contents to stdout
 *
 * fs_id = filesystem id
 * dir = directory path
 *
 * Returns E_OK if successful, an error code otherwise.
 */
static int dirlisting( const char *path )
{
    DIR             *dir;
    struct dirent   *dirent;
    struct stat     buf;
    char            path_to_file[PATH_MAX];

    dir = opendir(path);
    if ( dir != NULL )
    {
        printf( "--> Contents of folder \"%s\":\n", path );
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
                    printf("[DIR]\t%s/\n", dirent->d_name);
                }
                else if (S_ISREG(buf.st_mode))
                {
                    printf("[FILE]\t%s\t(%d kB)\n", dirent->d_name, (int)(buf.st_size / 1024) );
                }
                else
                {
                    printf("[UNK]\t%s\n", dirent->d_name);
                }
            }
            else
            {
                FAILED(E_STAT);
            }
        }
        printf( "<--\n\n" );

        if (closedir(dir) != 0)
        {
            FAILED(E_CLOSEDIR);
        }
        return E_OK;
    }
    return E_OPENDIR;
}

/* Copy files from one filesystem to another
 *
 * fs1_id = id source filesystem
 * srcdir = source directory
 * fs2_id = id destination filesystem
 * dstdir = destination directory (must exist)
 * pattern = pattern to match (with strstr()), e.g. "jpg"
 *
 * Returns E_OK if successful, an error code otherwise.
 */
static int copyfiles( const char *srcdirname, const char *dstdirname, const char *pattern )
{
    int                 status;
    DIR                 *srcdir;
    struct dirent       *srcdirent;
    char                dstfile[PATH_MAX];
    char                srcfile[PATH_MAX];
    struct stat         statbuf;
    int                 srcfd, dstfd;
    int                 size;
    int                 filesize;
    int                 bytes;
    uint8_t             buffer[BLOCKSIZE];
    int                 count = 0;
    int                 totalsize = 0;

    srcdir = opendir(srcdirname);
    if (srcdir == NULL)
    {
        return E_OPENDIR;
    }
    status = E_OK;
    do
    {
        srcdirent = readdir(srcdir);
        if (srcdirent == NULL)
        {
            break;
        }
        if ( !strstr( srcdirent->d_name, pattern ) )
        {
            continue;
        }
        strcpy(srcfile, srcdirname);
        strcat(srcfile, DIR_SEP);
        strcat(srcfile, srcdirent->d_name);
        if (stat(srcfile, &statbuf) != 0)
        {
            return E_STAT;
        }
        if (!S_ISREG(statbuf.st_mode))
        {
            continue;
        }

        srcfd = open(srcfile, O_RDONLY);
        if (srcfd < 0)
        {
            printf("failed to open source file '%s'\n", srcfile);
            continue;
        }

        strcpy(dstfile, dstdirname);
        strcat(dstfile, DIR_SEP);
        strcat(dstfile, srcdirent->d_name);
        dstfd = open(dstfile, O_RDWR | O_CREAT | O_TRUNC);
        if (dstfd < 0)
        {
            printf("failed to open destination file '%s'\n", dstfile);
            close(srcfd);
            continue;
        }

        size = statbuf.st_size;
//        printf( "++ copy \"%s\", size = %d bytes\n", srcdirent->d_name, size );
        for ( filesize = size; filesize > 0; filesize -= BLOCKSIZE )
        {
            bytes = read(srcfd, buffer, MIN( filesize, BLOCKSIZE ) );
            if ( bytes < 0 )
            {
                status = E_READ;
                break;
            }
            if ( write(dstfd, buffer, bytes ) < 0 )
            {
                status = E_WRITE;
                break;
            }
        }
        totalsize += size;
        count++;
        printf( "." );
        close(srcfd);
#if TODO
        fsync(dstfd);
#endif
        close(dstfd);
    } while ( status == E_OK && count < MAXCOPIES );
    printf( "\n++ Copied %d files\n", count );

    closedir(srcdir);

    return status;
}

/* Move files from one directory to another
 *
 * fs_id = filesystem pointer
 * fromdir = source directory
 * dstdir = destination directory
 * pattern = pattern to match (with strstr()), e.g. "jpg"
 *
 * Returns E_OK if successful, an error code otherwise.
 */
static int movefiles(const char *srcdirname, const char *dstdirname, const char *pattern )
{
    int                 status;
    DIR                 *srcdir;
    struct dirent       *srcdirent;
    char                dstfile[PATH_MAX];
    char                srcfile[PATH_MAX];
    struct stat         statbuf;
    bool                checkpattern = true;
    int                 count = 0;

    srcdir = opendir(srcdirname);
    if (srcdir == NULL)
    {
        return E_OPENDIR;
    }

    // check if dstdir is a directory...
    if (stat(dstdirname, &statbuf) != 0)
    {
        printf("unable to stat '%s'\n", dstdirname);
        closedir(srcdir);
        return E_STAT;
    }

    if ( pattern == NULL || strcmp( pattern, "*" ) == 0 )
    {
        checkpattern = false;
    }

    status = E_OK;
    while ( 1 )
    {
        srcdirent = readdir(srcdir);
        if (srcdirent == NULL)
        {
            break;
        }

        strcpy(srcfile, srcdirname);
        strcat(srcfile, DIR_SEP);
        strcat(srcfile, srcdirent->d_name);
        if (stat(srcfile, &statbuf) != 0)
        {
            printf("stat error '%s'\n", srcfile);
        }
        if (!S_ISREG(statbuf.st_mode))
        {
            continue;
        }

        // Check pattern
        if (checkpattern && !strstr(srcdirent->d_name, pattern))
        {
            continue;
        }

        strcpy(dstfile, dstdirname);
        strcat(dstfile, DIR_SEP);
        strcat(dstfile, srcdirent->d_name);
        if (rename(srcfile, dstfile) != 0)
        {
            status = E_RENAME;
            break;
        }
        count++;
        printf( "." );
    };
    printf( "\n++ Moved %d files\n", count );

    closedir(srcdir);

    return status;
}
