/*******************************************************************************

   CO_OD.c - Variables and Object Dictionary for CANopenNode

   Copyright (C) 2004  Janez Paternoster, Slovenia

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


   Author: janez.paternoster@siol.net


   This file was automatically generated with CANopenNode Object
   Dictionary Editor: http://canopennode.sourceforge.net/
   DON'T EDIT THIS FILE MANUALLY !!!!

*******************************************************************************/


#include "CANopen.h"


/*******************************************************************************
   DEFINITION AND INITIALIZATION OF OBJECT DICTIONARY VARIABLES
*******************************************************************************/

#ifdef __18CXX
   #pragma romdata CO_OD_RomVariables=0x1000 //ROM variables in PIC18fxxx must be above address 0x1000
#endif

/***** Definition for RAM variables *******************************************/
volatile struct sCO_OD_RAM CO_OD_RAM = {
           CO_OD_FIRST_LAST_BYTE,

/*1001*/ 0x0,
/*1002*/ 0x0L,
/*1003*/ {0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L},
/*2100*/ {0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00},
/*2103*/ 0x0,
/*2104*/ 0x0,
/*2105*/ {0x4, 0x0, 0x0, 0x0, 0x0},

           CO_OD_FIRST_LAST_BYTE,
};


/***** Definition for EEPROM variables ****************************************/
struct sCO_OD_EEPROM CO_OD_EEPROM = {
           CO_OD_FIRST_LAST_BYTE,

/*2106*/ 0x0L,

#if CO_NO_RETENTIVE_USAGE != 3  //if CO_NO_RETENTIVE_USAGE==3, then combine ROM variables into EEPROM memory space
           CO_OD_FIRST_LAST_BYTE,
};


/***** Definition for ROM variables *******************************************/
#if CO_NO_RETENTIVE_USAGE==0 || CO_NO_RETENTIVE_USAGE==2 || CO_NO_RETENTIVE_USAGE==4 || CO_NO_RETENTIVE_USAGE==6
   ROM struct sCO_OD_ROM CO_OD_ROM = {    //constant variables, stored in flash
#else
   struct sCO_OD_ROM CO_OD_ROM = {        //variables stored in RAM (and saved)
#endif
           CO_OD_FIRST_LAST_BYTE,
#endif

/*1000*/ 0x0L,
/*1005*/ 0x80L,
/*1006*/ 0x0L,
/*1007*/ 0x0L,
/*1008*/ {'C', 'A', 'N', 'o', 'p', 'e', 'n', 'N', 'o', 'd', 'e'},
/*1009*/ {'2', '.', '0', '0'},
/*100A*/ {'2', '.', '0', '0'},
/*1014*/ 0x80L,
/*1015*/ 0x0,
/*1016*/ {0x0L, 0x0L, 0x0L, 0x0L},
/*1017*/ 0x0,
/*1018*/ {0x4, 0x0L, 0x0L, 0x0L, 0x0L},
/*1029*/ {0x0},
/*1200*/{{0x2, 0x600L, 0x580L}},
/*1400*/{{0x2, 0x200L, 0xFE},
/*1401*/ {0x2, 0x300L, 0xFE},
/*1402*/ {0x2, 0x400L, 0xFE},
/*1403*/ {0x2, 0x500L, 0xFE}},
/*1600*/{{0x0, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L},
/*1601*/ {0x0, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L},
/*1602*/ {0x0, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L},
/*1603*/ {0x0, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L}},
/*1800*/{{0x5, 0x180L, 0xFE, 0x0, 0x0, 0x0},
/*1801*/ {0x5, 0x280L, 0xFE, 0x0, 0x0, 0x0},
/*1802*/ {0x5, 0x380L, 0xFE, 0x0, 0x0, 0x0},
/*1803*/ {0x5, 0x480L, 0xFE, 0x0, 0x0, 0x0}},
/*1A00*/{{0x0, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L},
/*1A01*/ {0x0, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L},
/*1A02*/ {0x0, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L},
/*1A03*/ {0x0, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L, 0x0L}},
/*1F80*/ 0x0L,
/*2101*/ 0x5,
/*2102*/ 0x5,

           CO_OD_FIRST_LAST_BYTE
};


#ifdef __18CXX
   #pragma romdata //return to the default section
#endif


/*******************************************************************************
   STRUCTURES FOR RECORD TYPE OBJECTS
*******************************************************************************/
/*0x1018*/ ROM ODrecord ODrecord1018[5] = {
           {(ROM void*)&CO_OD_ROM.identity.maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.identity.vendorID, 0x85,  4},
           {(ROM void*)&CO_OD_ROM.identity.productCode, 0x85,  4},
           {(ROM void*)&CO_OD_ROM.identity.revisionNumber, 0x85,  4},
           {(ROM void*)&CO_OD_ROM.identity.serialNumber, 0x85,  4}};
/*0x1200*/ ROM ODrecord ODrecord1200[3] = {
           {(ROM void*)&CO_OD_ROM.SDOServerParameter[0].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.SDOServerParameter[0].COB_IDClientToServer, 0x85,  4},
           {(ROM void*)&CO_OD_ROM.SDOServerParameter[0].COB_IDServerToClient, 0x85,  4}};
/*0x1400*/ ROM ODrecord ODrecord1400[3] = {
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[0].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[0].COB_IDUsedByRPDO, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[0].transmissionType, 0x0D,  1}};
/*0x1401*/ ROM ODrecord ODrecord1401[3] = {
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[1].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[1].COB_IDUsedByRPDO, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[1].transmissionType, 0x0D,  1}};
/*0x1402*/ ROM ODrecord ODrecord1402[3] = {
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[2].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[2].COB_IDUsedByRPDO, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[2].transmissionType, 0x0D,  1}};
/*0x1403*/ ROM ODrecord ODrecord1403[3] = {
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[3].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[3].COB_IDUsedByRPDO, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOCommunicationParameter[3].transmissionType, 0x0D,  1}};
/*0x1600*/ ROM ODrecord ODrecord1600[9] = {
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].numberOfMappedObjects, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].mappedObject1, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].mappedObject2, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].mappedObject3, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].mappedObject4, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].mappedObject5, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].mappedObject6, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].mappedObject7, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[0].mappedObject8, 0x8D,  4}};
/*0x1601*/ ROM ODrecord ODrecord1601[9] = {
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].numberOfMappedObjects, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].mappedObject1, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].mappedObject2, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].mappedObject3, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].mappedObject4, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].mappedObject5, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].mappedObject6, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].mappedObject7, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[1].mappedObject8, 0x8D,  4}};
/*0x1602*/ ROM ODrecord ODrecord1602[9] = {
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].numberOfMappedObjects, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].mappedObject1, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].mappedObject2, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].mappedObject3, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].mappedObject4, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].mappedObject5, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].mappedObject6, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].mappedObject7, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[2].mappedObject8, 0x8D,  4}};
/*0x1603*/ ROM ODrecord ODrecord1603[9] = {
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].numberOfMappedObjects, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].mappedObject1, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].mappedObject2, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].mappedObject3, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].mappedObject4, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].mappedObject5, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].mappedObject6, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].mappedObject7, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.RPDOMappingParameter[3].mappedObject8, 0x8D,  4}};
/*0x1800*/ ROM ODrecord ODrecord1800[6] = {
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[0].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[0].COB_IDUsedByTPDO, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[0].transmissionType, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[0].inhibitTime, 0x8D,  2},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[0].compatibilityEntry, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[0].eventTimer, 0x8D,  2}};
/*0x1801*/ ROM ODrecord ODrecord1801[6] = {
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[1].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[1].COB_IDUsedByTPDO, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[1].transmissionType, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[1].inhibitTime, 0x8D,  2},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[1].compatibilityEntry, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[1].eventTimer, 0x8D,  2}};
/*0x1802*/ ROM ODrecord ODrecord1802[6] = {
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[2].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[2].COB_IDUsedByTPDO, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[2].transmissionType, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[2].inhibitTime, 0x8D,  2},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[2].compatibilityEntry, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[2].eventTimer, 0x8D,  2}};
/*0x1803*/ ROM ODrecord ODrecord1803[6] = {
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[3].maxSubIndex, 0x05,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[3].COB_IDUsedByTPDO, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[3].transmissionType, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[3].inhibitTime, 0x8D,  2},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[3].compatibilityEntry, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOCommunicationParameter[3].eventTimer, 0x8D,  2}};
/*0x1A00*/ ROM ODrecord ODrecord1A00[9] = {
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].numberOfMappedObjects, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].mappedObject1, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].mappedObject2, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].mappedObject3, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].mappedObject4, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].mappedObject5, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].mappedObject6, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].mappedObject7, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[0].mappedObject8, 0x8D,  4}};
/*0x1A01*/ ROM ODrecord ODrecord1A01[9] = {
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].numberOfMappedObjects, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].mappedObject1, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].mappedObject2, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].mappedObject3, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].mappedObject4, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].mappedObject5, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].mappedObject6, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].mappedObject7, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[1].mappedObject8, 0x8D,  4}};
/*0x1A02*/ ROM ODrecord ODrecord1A02[9] = {
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].numberOfMappedObjects, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].mappedObject1, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].mappedObject2, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].mappedObject3, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].mappedObject4, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].mappedObject5, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].mappedObject6, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].mappedObject7, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[2].mappedObject8, 0x8D,  4}};
/*0x1A03*/ ROM ODrecord ODrecord1A03[9] = {
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].numberOfMappedObjects, 0x0D,  1},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].mappedObject1, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].mappedObject2, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].mappedObject3, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].mappedObject4, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].mappedObject5, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].mappedObject6, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].mappedObject7, 0x8D,  4},
           {(ROM void*)&CO_OD_ROM.TPDOMappingParameter[3].mappedObject8, 0x8D,  4}};
/*0x2105*/ ROM ODrecord ODrecord2105[5] = {
           {(ROM void*)&CO_OD_RAM.performance.maxSubIndex, 0x06,  1},
           {(ROM void*)&CO_OD_RAM.performance.timerBandwidthPerc, 0x06,  1},
           {(ROM void*)&CO_OD_RAM.performance.timerBandwidthPercMax, 0x06,  1},
           {(ROM void*)&CO_OD_RAM.performance.mainlineCountPer100Ms, 0x86,  2},
           {(ROM void*)&CO_OD_RAM.performance.mainlineCountPer100MsMax, 0x86,  2}};


/*******************************************************************************
   SDO SERVER ACCESS FUNCTIONS WITH USER CODE
*******************************************************************************/
#define READING (dir==0)
#define WRITING (dir==1)

UNSIGNED32 CO_ODF(UNSIGNED16, UNSIGNED8, unsigned char, unsigned char, unsigned char, void*, ROM void*);

UNSIGNED32 CO_ODF_1003(UNSIGNED16, UNSIGNED8, unsigned char, unsigned char, unsigned char, void*, ROM void*);

UNSIGNED32 CO_ODF_1014(UNSIGNED16 index, UNSIGNED8 subindex, unsigned char attribute, unsigned char length, unsigned char dir, void* dataBuff, ROM void* pData){
  UNSIGNED32 abortCode;
  extern UNSIGNED8 CO_NodeID;
  abortCode = CO_ODF(index, subindex, attribute, length, dir, dataBuff, pData);
  *((UNSIGNED32 *) dataBuff) += CO_NodeID;
  return abortCode;
}

UNSIGNED32 CO_ODF_1016(UNSIGNED16, UNSIGNED8, unsigned char, unsigned char, unsigned char, void*, ROM void*);

UNSIGNED32 CO_ODF_1200(UNSIGNED16 index, UNSIGNED8 subindex, unsigned char attribute, unsigned char length, unsigned char dir, void* dataBuff, ROM void* pData){
  UNSIGNED32 abortCode;
  extern UNSIGNED8 CO_NodeID;
  abortCode = CO_ODF(index, subindex, attribute, length, dir, dataBuff, pData);
  if(subindex>0) *((UNSIGNED32 *) dataBuff) += CO_NodeID;
  return abortCode;
}

UNSIGNED32 CO_ODF_RPDOcom(UNSIGNED16, UNSIGNED8, unsigned char, unsigned char, unsigned char, void*, ROM void*);

UNSIGNED32 CO_ODF_RPDOmap(UNSIGNED16, UNSIGNED8, unsigned char, unsigned char, unsigned char, void*, ROM void*);

UNSIGNED32 CO_ODF_TPDOcom(UNSIGNED16, UNSIGNED8, unsigned char, unsigned char, unsigned char, void*, ROM void*);

UNSIGNED32 CO_ODF_TPDOmap(UNSIGNED16, UNSIGNED8, unsigned char, unsigned char, unsigned char, void*, ROM void*);

UNSIGNED32 CO_ODF_2101(UNSIGNED16 index, UNSIGNED8 subindex, unsigned char attribute, unsigned char length, unsigned char dir, void* dataBuff, ROM void* pData){
  UNSIGNED32 abortCode;
  if(WRITING){
    unsigned char var = *((unsigned char*)dataBuff);
    if(var < 1) return 0x06090032L;  //Value of parameter written too low.
    if(var > 127) return 0x06090031L;  //Value of parameter written too high.
  }
  abortCode = CO_ODF(index, subindex, attribute, length, dir, dataBuff, pData);
  return abortCode;
}

UNSIGNED32 CO_ODF_2102(UNSIGNED16 index, UNSIGNED8 subindex, unsigned char attribute, unsigned char length, unsigned char dir, void* dataBuff, ROM void* pData){
  UNSIGNED32 abortCode;
  if(WRITING){
    unsigned char var = *((unsigned char*)dataBuff);
    if(var > 7) return 0x06090031L;  //Value of parameter written too high.
  }
  abortCode = CO_ODF(index, subindex, attribute, length, dir, dataBuff, pData);
  return abortCode;
}


/*******************************************************************************
   OBJECT DICTIONARY
*******************************************************************************/
ROM sCO_OD_object CO_OD[] = {
{0x1000, 0x01, 0x85,  4, (ROM void*)&CO_OD_ROM.deviceType,                            CO_ODF},
{0x1001, 0x01, 0x36,  1, (ROM void*)&CO_OD_RAM.errorRegister,                         CO_ODF},
{0x1002, 0x01, 0xB6,  4, (ROM void*)&CO_OD_RAM.manufacturerStatusRegister,            CO_ODF},
{0x1003, 0x09, 0x8E,  4, (ROM void*)&CO_OD_RAM.preDefinedErrorField[0],               CO_ODF_1003},
{0x1005, 0x01, 0x8D,  4, (ROM void*)&CO_OD_ROM.COB_ID_SYNCMessage,                    CO_ODF},
{0x1006, 0x01, 0x8D,  4, (ROM void*)&CO_OD_ROM.communicationCyclePeriod,              CO_ODF},
{0x1007, 0x01, 0x8D,  4, (ROM void*)&CO_OD_ROM.synchronousWindowLength,               CO_ODF},
{0x1008, 0x01, 0x05, 11, (ROM void*)&CO_OD_ROM.manufacturerDeviceName[0],             CO_ODF},
{0x1009, 0x01, 0x05,  4, (ROM void*)&CO_OD_ROM.manufacturerHardwareVersion[0],        CO_ODF},
{0x100A, 0x01, 0x05,  4, (ROM void*)&CO_OD_ROM.manufacturerSoftwareVersion[0],        CO_ODF},
{0x1014, 0x01, 0x85,  4, (ROM void*)&CO_OD_ROM.COB_ID_EMCY,                           CO_ODF_1014},
{0x1015, 0x01, 0x8D,  2, (ROM void*)&CO_OD_ROM.inhibitTimeEMCY,                       CO_ODF},
{0x1016, 0x05, 0x8D,  4, (ROM void*)&CO_OD_ROM.consumerHeartbeatTime[0],              CO_ODF_1016},
{0x1017, 0x01, 0x8D,  2, (ROM void*)&CO_OD_ROM.producerHeartbeatTime,                 CO_ODF},
{0x1018, 0x05, 0x00,  0, (ROM void*)&ODrecord1018,                                    CO_ODF},
{0x1029, 0x02, 0x0D,  1, (ROM void*)&CO_OD_ROM.errorBehavior[0],                      CO_ODF},
{0x1200, 0x03, 0x00,  0, (ROM void*)&ODrecord1200,                                    CO_ODF_1200},
{0x1400, 0x03, 0x00,  0, (ROM void*)&ODrecord1400,                                    CO_ODF_RPDOcom},
{0x1401, 0x03, 0x00,  0, (ROM void*)&ODrecord1401,                                    CO_ODF_RPDOcom},
{0x1402, 0x03, 0x00,  0, (ROM void*)&ODrecord1402,                                    CO_ODF_RPDOcom},
{0x1403, 0x03, 0x00,  0, (ROM void*)&ODrecord1403,                                    CO_ODF_RPDOcom},
{0x1600, 0x09, 0x00,  0, (ROM void*)&ODrecord1600,                                    CO_ODF_RPDOmap},
{0x1601, 0x09, 0x00,  0, (ROM void*)&ODrecord1601,                                    CO_ODF_RPDOmap},
{0x1602, 0x09, 0x00,  0, (ROM void*)&ODrecord1602,                                    CO_ODF_RPDOmap},
{0x1603, 0x09, 0x00,  0, (ROM void*)&ODrecord1603,                                    CO_ODF_RPDOmap},
{0x1800, 0x06, 0x00,  0, (ROM void*)&ODrecord1800,                                    CO_ODF_TPDOcom},
{0x1801, 0x06, 0x00,  0, (ROM void*)&ODrecord1801,                                    CO_ODF_TPDOcom},
{0x1802, 0x06, 0x00,  0, (ROM void*)&ODrecord1802,                                    CO_ODF_TPDOcom},
{0x1803, 0x06, 0x00,  0, (ROM void*)&ODrecord1803,                                    CO_ODF_TPDOcom},
{0x1A00, 0x09, 0x00,  0, (ROM void*)&ODrecord1A00,                                    CO_ODF_TPDOmap},
{0x1A01, 0x09, 0x00,  0, (ROM void*)&ODrecord1A01,                                    CO_ODF_TPDOmap},
{0x1A02, 0x09, 0x00,  0, (ROM void*)&ODrecord1A02,                                    CO_ODF_TPDOmap},
{0x1A03, 0x09, 0x00,  0, (ROM void*)&ODrecord1A03,                                    CO_ODF_TPDOmap},
{0x1F80, 0x01, 0x8D,  4, (ROM void*)&CO_OD_ROM.NMTStartup,                            CO_ODF},
{0x2100, 0x01, 0x36,  8, (ROM void*)&CO_OD_RAM.errorStatusBits[0],                    CO_ODF},
{0x2101, 0x01, 0x0D,  1, (ROM void*)&CO_OD_ROM.CANNodeID,                             CO_ODF_2101},
{0x2102, 0x01, 0x0D,  1, (ROM void*)&CO_OD_ROM.CANBitRate,                            CO_ODF_2102},
{0x2103, 0x01, 0x8E,  2, (ROM void*)&CO_OD_RAM.SYNCCounter,                           CO_ODF},
{0x2104, 0x01, 0x86,  2, (ROM void*)&CO_OD_RAM.SYNCTime,                              CO_ODF},
{0x2105, 0x05, 0x00,  0, (ROM void*)&ODrecord2105,                                    CO_ODF},
{0x2106, 0x01, 0x87,  4, (ROM void*)&CO_OD_EEPROM.powerOnCounter,                     CO_ODF},
};

/***** Number of Elements in Object Dictionary ********************************/
ROM unsigned int CO_OD_NoOfElements = sizeof(CO_OD) / sizeof(CO_OD[0]);

