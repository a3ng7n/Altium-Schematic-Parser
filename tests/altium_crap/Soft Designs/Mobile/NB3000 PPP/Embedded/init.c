#include <assert.h>
#include "swplatform.h"
#define NSEC_PER_MSEC 1000000
#define SIM_RETRIES 5

#define PPP_RUNTIME_CONFIGURE 1

static struct timespec half_second      = {0, 500 * NSEC_PER_MSEC};

/* Returns true if 2g modem found, false if 3g modem found */
bool find_modem()
{
    modem_t * modem_2g      = modem_open(MODEM_2G);
    modem_t * modem_3g      = modem_open(MODEM_3G);

    puts("Waiting for modem...");

    if (modem_cmd(modem_2g,"AT"))
        return true;
    if (modem_cmd(modem_3g,"AT"))
        return false;

    cellular_pwr_on(cellular_open(DRV_CELLULAR_2G));
    cellular_pwr_on(cellular_open(DRV_CELLULAR_3G));

    for(;;)
    {
        if (modem_cmd(modem_2g,"AT"))
            return true;
        if (modem_cmd(modem_3g,"AT"))
            return false;
        nanosleep(&half_second,NULL);
    }
}

bool init(modem_t ** modem, lwip_t ** lwip)
{
    bool      is_2g;
    modem_cme_error_t error;
    modem_status_t status, status_last = MODEM_STATUS_INVALID;
    ppp_t * ppp;

    assert(modem && lwip);

    is_2g  = find_modem();
    *modem = modem_open(is_2g ? MODEM_2G : MODEM_3G);
    *lwip  = lwip_open(is_2g ? IF_2G : IF_3G);
    ppp    = ppp_open(is_2g ? PPP_2G : PPP_3G);
    printf("Found %s modem.\n", is_2g ? "2G" : "3G");

    while(!modem_start(*modem));

    puts("Waiting for network...");
    while (!modem_check_network(*modem))
    {
      status = modem_network_status(*modem);
      if (status!=status_last)
        switch(status)
        {
        default:
        case MODEM_STATUS_IDLE:
            if ((error = modem_sim_status(*modem)) == CME_OK)
                puts("  Network down - is antenna connected?");
            else
            {
                printf("  SIM error: %s.", modem_cme_error_str(error));
                fflush(stdout);
            }
            break;
        case MODEM_STATUS_SEARCHING:
            puts("  Searching for cell...");
            break;
        }
        status_last = status;
        nanosleep(&half_second,NULL);
    }
    puts("  Cell found.");

#if PPP_RUNTIME_CONFIGURE
    ifconfig_ppp_set_credentials (ppp, "", "****");
    ifconfig_ppp_set_access_point(ppp, "vfinternet.au");
    ifconfig_ppp_set_retry_count (ppp, 3);
#endif

    fputs("Connecting PPP...",stdout);
     while (lwip_start(*lwip)!=ERR_OK)
    {
        putc('.',stdout);
        fflush(stdout);
        lwip_stop(*lwip);
        nanosleep(&half_second,NULL);
    }
    putc('\n',stdout);
    return is_2g;
}




