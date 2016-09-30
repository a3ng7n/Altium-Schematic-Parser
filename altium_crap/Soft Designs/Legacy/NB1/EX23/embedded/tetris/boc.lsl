// FILE: @(#)boc.lsl	1.3 04/10/11
// DESCRIPTION:
//	Linker descriptor file for BoC based projects

memory iram
{
    mau = 8;
    type = ram;
    size = 256;
    map ( dest=bus:c51:idata_bus, src_offset=0x0, dest_offset=0x0, size=256 );
}
memory xram
{
    mau = 8;
    type = ram;
    size = 64k;
    map ( dest=bus:c51:xdata_bus, src_offset=0x0, dest_offset=0x0, size=64k );
}
memory xrom
{
    mau = 8;
    type = rom;
    size = 64k;
    map ( dest=bus:c51:code_bus, src_offset=0x0, dest_offset=0x0, size=64k );
}
section_layout c51:c51:idata {
    group (run_addr = 0xd9, ordered, contiguous) reserved "reserved-0xd9" (size = 6);
}
section_layout c51:c51:xdata {
    group (run_addr = 0, ordered, contiguous) reserved "reserved-0" (size = 0x400);
}
#define __STACK		0x20
#define __HEAP		0x200
#define __HEAP_FIXED	true
#define __VSTACK_XDATA	0x400
#define __VSTACK_FIXED	true
#define __XHEAP		__HEAP
#define __XPAGE		0x00
#define __MEMORY

#include "51.lsl"
