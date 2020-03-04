/*******************************************************************************

   CO_OD.h - Header for variables and Object Dictionary for CANopenNode

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

#ifndef _CO_OD_H
#define _CO_OD_H

#include <co_defs.h>
/*******************************************************************************
   DEVICE INFO:
      VendorName:     Paternoster
      VendorNumber:   0
      ProductName:    CANopenNode
      ProductNumber:  0
*******************************************************************************/


/*******************************************************************************
   FEATURES
*******************************************************************************/
   #define CO_NO_SYNC                     1   //Associated objects: 1005, 1006, 1007, 2103, 2104
   #define CO_NO_EMERGENCY                1   //Associated objects: 1014, 1015
   #define CO_NO_SDO_SERVER               1   //Associated objects: 1200..1200
   #define CO_NO_SDO_CLIENT               0   
   #define CO_NO_RPDO                     4   //Associated objects: 1400..1403, 1600..1603
   #define CO_NO_TPDO                     4   //Associated objects: 1800..1803, 1A00..1A03
   #define CO_NO_USER_CANRX               0   
   #define CO_NO_USER_CANTX               0   
   #define CO_NO_MAX_OBJECT_SIZE          20  
   #define CO_NO_RETENTIVE_USAGE          4   
   #define CO_NO_RPDO_USAGE               11  
   #define CO_NO_TPDO_USAGE               11  


/*******************************************************************************
   CANOPEN BASIC DATA TYPES AND OBJECT DICTIONARY DEFINITIONS
*******************************************************************************/
/* One object in Object Dictionary */
   typedef struct {
      UNSIGNED16       index;
      UNSIGNED8        subNumber;
      unsigned char    attribute;
      unsigned char    length;
      ROM void*        pData;
      UNSIGNED32       (*pFunct)(UNSIGNED16 index, UNSIGNED8 subindex, unsigned char attribute,
                       unsigned char length, unsigned char dir, void* dataBuff, ROM void* pData);
   } sCO_OD_object;

/* Attributes for Object Dictionary objects (flags) */
   #define CO_ODA_MEM_ROM          0x01
   #define CO_ODA_MEM_RAM          0x02
   #define CO_ODA_MEM_EEPROM       0x03
   #define CO_ODA_READABLE         0x04
   #define CO_ODA_WRITEABLE        0x08
   #define CO_ODA_RPDO_MAPABLE     0x10
   #define CO_ODA_TPDO_MAPABLE     0x20
   #define CO_ODA_TPDO_REQUESTABLE 0x40
   #define CO_ODA_MB_VALUE         0x80 // True when entry is a multibyte value

/* Structure for record type objects */
   typedef struct {ROM void* pData; unsigned char attribute; unsigned char length;} ODrecord;

/* Object Dictionary array */
   extern ROM sCO_OD_object CO_OD[];
   extern ROM unsigned int CO_OD_NoOfElements;


/*******************************************************************************
   TYPE DEFINITIONS FOR RECORDS
*******************************************************************************/
/*1018      */ typedef struct{
               UNSIGNED8      maxSubIndex;
               UNSIGNED32     vendorID;
               UNSIGNED32     productCode;
               UNSIGNED32     revisionNumber;
               UNSIGNED32     serialNumber;
               }              ODs_identity;

/*1200..1200*/ typedef struct{
               UNSIGNED8      maxSubIndex;
               UNSIGNED32     COB_IDClientToServer;
               UNSIGNED32     COB_IDServerToClient;
               }              ODs_SDOServerParameter;

/*1400..1403*/ typedef struct{
               UNSIGNED8      maxSubIndex;
               UNSIGNED32     COB_IDUsedByRPDO;
               UNSIGNED8      transmissionType;
               }              ODs_RPDOCommunicationParameter;

/*1600..1603*/ typedef struct{
               UNSIGNED8      numberOfMappedObjects;
               UNSIGNED32     mappedObject1;
               UNSIGNED32     mappedObject2;
               UNSIGNED32     mappedObject3;
               UNSIGNED32     mappedObject4;
               UNSIGNED32     mappedObject5;
               UNSIGNED32     mappedObject6;
               UNSIGNED32     mappedObject7;
               UNSIGNED32     mappedObject8;
               }              ODs_RPDOMappingParameter;

/*1800..1803*/ typedef struct{
               UNSIGNED8      maxSubIndex;
               UNSIGNED32     COB_IDUsedByTPDO;
               UNSIGNED8      transmissionType;
               UNSIGNED16     inhibitTime;
               UNSIGNED8      compatibilityEntry;
               UNSIGNED16     eventTimer;
               }              ODs_TPDOCommunicationParameter;

/*1A00..1A03*/ typedef struct{
               UNSIGNED8      numberOfMappedObjects;
               UNSIGNED32     mappedObject1;
               UNSIGNED32     mappedObject2;
               UNSIGNED32     mappedObject3;
               UNSIGNED32     mappedObject4;
               UNSIGNED32     mappedObject5;
               UNSIGNED32     mappedObject6;
               UNSIGNED32     mappedObject7;
               UNSIGNED32     mappedObject8;
               }              ODs_TPDOMappingParameter;

/*2105      */ typedef struct{
               UNSIGNED8      maxSubIndex;
               UNSIGNED8      timerBandwidthPerc;
               UNSIGNED8      timerBandwidthPercMax;
               UNSIGNED16     mainlineCountPer100Ms;
               UNSIGNED16     mainlineCountPer100MsMax;
               }              ODs_performance;


/*******************************************************************************
   STRUCTURES FOR VARIABLES IN DIFFERENT MEMORY LOCATIONS
*******************************************************************************/
#define  CO_OD_FIRST_LAST_BYTE     0x55 //Any value from 0x01 to 0xFE. If changed, EEPROM will be reinitialized.

/***** Structure for RAM variables ********************************************/
struct sCO_OD_RAM{
           unsigned char FirstByte;

/*1001      */ UNSIGNED8      errorRegister;
/*1002      */ UNSIGNED32     manufacturerStatusRegister;
/*1003      */ UNSIGNED32     preDefinedErrorField[8];
/*2100      */ OCTET_STRING   errorStatusBits[8];
/*2103      */ UNSIGNED16     SYNCCounter;
/*2104      */ UNSIGNED16     SYNCTime;
/*2105      */ ODs_performance performance;

           unsigned int LastByte;
};

/***** Structure for EEPROM variables *****************************************/
struct sCO_OD_EEPROM{
           unsigned char FirstByte;

/*2106      */ UNSIGNED32     powerOnCounter;

#if CO_NO_RETENTIVE_USAGE != 3  //if CO_NO_RETENTIVE_USAGE==3, then combine ROM variables into EEPROM memory space
           unsigned int LastByte;
};


/***** Structure for ROM variables ********************************************/
struct sCO_OD_ROM{
           unsigned char FirstByte;
#endif

/*1000      */ UNSIGNED32     deviceType;
/*1005      */ UNSIGNED32     COB_ID_SYNCMessage;
/*1006      */ UNSIGNED32     communicationCyclePeriod;
/*1007      */ UNSIGNED32     synchronousWindowLength;
/*1008      */ VISIBLE_STRING manufacturerDeviceName[11];
/*1009      */ VISIBLE_STRING manufacturerHardwareVersion[4];
/*100A      */ VISIBLE_STRING manufacturerSoftwareVersion[4];
/*1014      */ UNSIGNED32     COB_ID_EMCY;
/*1015      */ UNSIGNED16     inhibitTimeEMCY;
/*1016      */ UNSIGNED32     consumerHeartbeatTime[4];
/*1017      */ UNSIGNED16     producerHeartbeatTime;
/*1018      */ ODs_identity   identity;
/*1029      */ UNSIGNED8      errorBehavior[1];
/*1200..1200*/ ODs_SDOServerParameter SDOServerParameter[1];
/*1400..1403*/ ODs_RPDOCommunicationParameter RPDOCommunicationParameter[4];
/*1600..1603*/ ODs_RPDOMappingParameter RPDOMappingParameter[4];
/*1800..1803*/ ODs_TPDOCommunicationParameter TPDOCommunicationParameter[4];
/*1A00..1A03*/ ODs_TPDOMappingParameter TPDOMappingParameter[4];
/*1F80      */ UNSIGNED32     NMTStartup;
/*2101      */ UNSIGNED8      CANNodeID;
/*2102      */ UNSIGNED8      CANBitRate;

           unsigned int LastByte;
};


/***** Declaration of Object Dictionary variables *****************************/
extern volatile struct sCO_OD_RAM CO_OD_RAM;

extern struct sCO_OD_EEPROM CO_OD_EEPROM;

//Memory location of ROM variables can be in RAM or in Flash
//if CO_NO_RETENTIVE_USAGE==3, then ROM variables are combined with EEPROM variables
#if CO_NO_RETENTIVE_USAGE != 3
   #if CO_NO_RETENTIVE_USAGE==0 || CO_NO_RETENTIVE_USAGE==2 || CO_NO_RETENTIVE_USAGE==4 || CO_NO_RETENTIVE_USAGE==6
      extern ROM struct sCO_OD_ROM CO_OD_ROM;
   #else
      extern struct sCO_OD_ROM CO_OD_ROM;
   #endif
#else
   #define CO_OD_ROM CO_OD_EEPROM
#endif


/*******************************************************************************
   ALIASES FOR OBJECT DICTIONARY VARIABLES
*******************************************************************************/
/*1000, Data Type: UNSIGNED32 */
      #define OD_deviceType                              CO_OD_ROM.deviceType

/*1001, Data Type: UNSIGNED8 */
      #define OD_errorRegister                           CO_OD_RAM.errorRegister

/*1002, Data Type: UNSIGNED32 */
      #define OD_manufacturerStatusRegister              CO_OD_RAM.manufacturerStatusRegister

/*1003, Data Type: UNSIGNED32, Array[8] */
      #define OD_preDefinedErrorField                    CO_OD_RAM.preDefinedErrorField
      #define ODL_preDefinedErrorField_arrayLength       8

/*1005, Data Type: UNSIGNED32 */
      #define OD_COB_ID_SYNCMessage                      CO_OD_ROM.COB_ID_SYNCMessage

/*1006, Data Type: UNSIGNED32 */
      #define OD_communicationCyclePeriod                CO_OD_ROM.communicationCyclePeriod

/*1007, Data Type: UNSIGNED32 */
      #define OD_synchronousWindowLength                 CO_OD_ROM.synchronousWindowLength

/*1008, Data Type: VISIBLE_STRING, Array[11] */
      #define OD_manufacturerDeviceName                  CO_OD_ROM.manufacturerDeviceName
      #define ODL_manufacturerDeviceName_stringLength    11

/*1009, Data Type: VISIBLE_STRING, Array[4] */
      #define OD_manufacturerHardwareVersion             CO_OD_ROM.manufacturerHardwareVersion
      #define ODL_manufacturerHardwareVersion_stringLength 4

/*100A, Data Type: VISIBLE_STRING, Array[4] */
      #define OD_manufacturerSoftwareVersion             CO_OD_ROM.manufacturerSoftwareVersion
      #define ODL_manufacturerSoftwareVersion_stringLength 4

/*1014, Data Type: UNSIGNED32 */
      #define OD_COB_ID_EMCY                             CO_OD_ROM.COB_ID_EMCY

/*1015, Data Type: UNSIGNED16 */
      #define OD_inhibitTimeEMCY                         CO_OD_ROM.inhibitTimeEMCY

/*1016, Data Type: UNSIGNED32, Array[4] */
      #define OD_consumerHeartbeatTime                   CO_OD_ROM.consumerHeartbeatTime
      #define ODL_consumerHeartbeatTime_arrayLength      4

/*1017, Data Type: UNSIGNED16 */
      #define OD_producerHeartbeatTime                   CO_OD_ROM.producerHeartbeatTime

/*1018, Data Type: ODs_identity */
      #define OD_identity                                CO_OD_ROM.identity

/*1029, Data Type: UNSIGNED8, Array[1] */
      #define OD_errorBehavior                           CO_OD_ROM.errorBehavior
      #define ODL_errorBehavior_arrayLength              1
      #define ODA_errorBehavior_communicationError       0

/*1200..1200, Data Type: ODs_SDOServerParameter, Array[1] */
      #define OD_SDOServerParameter                      CO_OD_ROM.SDOServerParameter

/*1400..1403, Data Type: ODs_RPDOCommunicationParameter, Array[4] */
      #define OD_RPDOCommunicationParameter              CO_OD_ROM.RPDOCommunicationParameter

/*1600..1603, Data Type: ODs_RPDOMappingParameter, Array[4] */
      #define OD_RPDOMappingParameter                    CO_OD_ROM.RPDOMappingParameter

/*1800..1803, Data Type: ODs_TPDOCommunicationParameter, Array[4] */
      #define OD_TPDOCommunicationParameter              CO_OD_ROM.TPDOCommunicationParameter

/*1A00..1A03, Data Type: ODs_TPDOMappingParameter, Array[4] */
      #define OD_TPDOMappingParameter                    CO_OD_ROM.TPDOMappingParameter

/*1F80, Data Type: UNSIGNED32 */
      #define OD_NMTStartup                              CO_OD_ROM.NMTStartup

/*2100, Data Type: OCTET_STRING, Array[8] */
      #define OD_errorStatusBits                         CO_OD_RAM.errorStatusBits
      #define ODL_errorStatusBits_stringLength           8

/*2101, Data Type: UNSIGNED8 */
      #define OD_CANNodeID                               CO_OD_ROM.CANNodeID

/*2102, Data Type: UNSIGNED8 */
      #define OD_CANBitRate                              CO_OD_ROM.CANBitRate

/*2103, Data Type: UNSIGNED16 */
      #define OD_SYNCCounter                             CO_OD_RAM.SYNCCounter

/*2104, Data Type: UNSIGNED16 */
      #define OD_SYNCTime                                CO_OD_RAM.SYNCTime

/*2105, Data Type: ODs_performance */
      #define OD_performance                             CO_OD_RAM.performance

/*2106, Data Type: UNSIGNED32 */
      #define OD_powerOnCounter                          CO_OD_EEPROM.powerOnCounter


#endif
