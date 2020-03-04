/******************************************************************************
 * FILE:	@(#)simin.c	1.5 04/09/22
 * DESCRIPTION:
 *	Simulate 'user' input device.
 *****************************************************************************/
#if defined(DEMO)
#include "demo.h"
#else
#include "tetris.h"
#endif

#if defined(SIMINRX)

DeclareAlarm(asiminnk);
DeclareEvent(esiminrx);
DeclareEvent(esiminnk);
DeclareMessage(mrsiminrx);
DeclareMessage(mssiminrx);
DeclareTask(tsiminrx);

static char	next_char;

struct keys
{
	unsigned char time;
	char key;
};

#ifdef TETRIS
#define M_NOP	-1

static struct keys ai[] = {
	{1, M_ROTATE}, {1, M_ROTATE}, {1, M_ROTATE}, {1, M_DOWN}, /* mirrored L */
	{2, M_ROTATE}, {2, M_ROTATE}, {2, M_ROTATE}, {2, M_LEFT}, {2, M_LEFT}, {2, M_LEFT}, {2, M_DOWN}, /* L */
	{3, M_ROTATE}, {3, M_ROTATE}, {3, M_ROTATE}, {3, M_RIGHT}, {3, M_RIGHT}, {3, M_RIGHT}, {3, M_DOWN}, /* T */
	{4, M_ROTATE}, {4, M_RIGHT}, {4, M_RIGHT}, {4, M_RIGHT}, {4, M_RIGHT}, {4, M_RIGHT}, {4, M_RIGHT}, {4, M_DOWN}, /* I */
	{5, M_ROTATE}, {5, M_DOWN}, /* Z */
	{6, M_ROTATE}, {6, M_LEFT}, {6, M_LEFT}, {6, M_LEFT}, {6, M_LEFT}, {6, M_LEFT}, {6, M_DOWN}, /* S */
	{7, M_RIGHT}, {7, M_RIGHT}, {7, M_RIGHT}, {7, M_RIGHT}, {7, M_RIGHT}, {7, M_RIGHT}, {7, M_RIGHT}, {7, M_DOWN}, /* square */
	{8, M_ROTATE}, {8, M_ROTATE}, {8, M_RIGHT}, {8, M_RIGHT}, {8, M_RIGHT}, {8, M_RIGHT}, {8, M_RIGHT}, {8, M_DOWN}, /* mirrored L */
	{9, M_ROTATE}, {9, M_LEFT}, {9, M_LEFT}, {9, M_LEFT}, {9, M_LEFT}, {9, M_LEFT}, {9, M_DOWN}, /* L */
	{10, M_ROTATE}, {10, M_RIGHT}, {10, M_RIGHT}, {10, M_DOWN}, /* T */
	{11, M_LEFT}, {11, M_LEFT}, {11, M_LEFT}, {11, M_DOWN}, /* I */
	{12, M_LEFT}, {12, M_DOWN}, /* Z */
	{13, M_RIGHT}, {13, M_RIGHT}, {13, M_RIGHT}, {13, M_RIGHT}, {13, M_DOWN}, /* S */
	{14, M_RIGHT}, {14, M_RIGHT}, {14, M_RIGHT}, {14, M_RIGHT}, {14, M_RIGHT}, {14, M_RIGHT}, {14, M_RIGHT}, {14, M_DOWN}, /* square */
	{15, M_ROTATE}, {15, M_ROTATE}, {15, M_ROTATE}, {15, M_LEFT}, {15, M_LEFT}, {15, M_LEFT}, {15, M_LEFT}, {15, M_LEFT}, {15, M_DOWN}, /* mirrored L */
	{16, M_ROTATE}, {16, M_ROTATE}, {16, M_ROTATE}, {16, M_RIGHT}, {16, M_DOWN}, /* L */
	{17, M_ROTATE}, {17, M_ROTATE}, {17, M_ROTATE}, {17, M_LEFT}, {17, M_LEFT}, {17, M_LEFT}, {17, M_DOWN}, /* T */
	{18, M_LEFT}, {18, M_LEFT}, {18, M_LEFT}, {18, M_LEFT}, {18, M_LEFT}, {18, M_LEFT}, {18, M_DOWN}, /* I */
	{19, M_ROTATE}, {19, M_DOWN}, /* Z */
	{20, M_ROTATE}, {20, M_RIGHT}, {20, M_RIGHT}, {20, M_RIGHT}, {20, M_RIGHT}, {20, M_RIGHT}, {20, M_DOWN}, /* S */
	{21, M_RIGHT}, {21, M_RIGHT}, {21, M_DOWN}, /* square */
	{22, M_ROTATE}, {22, M_ROTATE}, {22, M_RIGHT}, {22, M_RIGHT}, {22, M_RIGHT}, {22, M_RIGHT}, {22, M_DOWN}, /* mirrored L */
	{23, M_ROTATE}, {23, M_LEFT}, {23, M_DOWN}, /* L */
	{24, M_LEFT}, {24, M_LEFT}, {24, M_LEFT}, {24, M_LEFT}, {24, M_DOWN}, /* T */
	{25, M_RIGHT}, {25, M_RIGHT}, {25, M_RIGHT}, {25, M_RIGHT}, {25, M_RIGHT}, {25, M_RIGHT}, {25, M_RIGHT}, {25, M_DOWN}, /* I */
	{26, M_ROTATE}, {26, M_LEFT}, {26, M_LEFT}, {26, M_LEFT}, {26, M_LEFT}, {26, M_DOWN}, /* Z */
	{27, M_RIGHT}, {27, M_RIGHT}, {27, M_RIGHT}, {27, M_DOWN}, /* S */
	{28, M_RIGHT}, {28, M_RIGHT}, {28, M_RIGHT}, {28, M_RIGHT}, {28, M_RIGHT}, {28, M_RIGHT}, {28, M_DOWN}, /* square */
	{29, M_ROTATE}, {29, M_ROTATE}, {29, M_ROTATE}, {29, M_LEFT}, {29, M_LEFT}, {29, M_DOWN}, /* mirrored L */
	{30, M_RIGHT}, {30, M_DOWN}, /* L */
	{31, M_ROTATE}, {31, M_ROTATE}, {31, M_ROTATE}, {31, M_LEFT}, {31, M_LEFT}, {31, M_LEFT}, {31, M_LEFT}, {31, M_LEFT}, {31, M_DOWN}, /* T */
	{32, M_ROTATE}, {32, M_RIGHT}, {32, M_RIGHT}, {32, M_RIGHT}, {32, M_RIGHT}, {32, M_RIGHT}, {32, M_DOWN}, /* I */

	{33, M_ROTATE}, {33, M_LEFT}, {33, M_LEFT}, {33, M_DOWN}, /* Z */
	{34, M_RIGHT}, {34, M_RIGHT}, {34, M_RIGHT}, {34, M_RIGHT}, {34, M_RIGHT}, {34, M_RIGHT}, {34, M_RIGHT}, {34, M_DOWN}, /* S */
	{35, M_RIGHT}, {35, M_RIGHT}, {35, M_RIGHT}, {35, M_RIGHT}, {35, M_RIGHT}, {35, M_DOWN}, /* square */
	{36, M_ROTATE}, {36, M_RIGHT}, {36, M_RIGHT}, {36, M_RIGHT}, {36, M_RIGHT}, {36, M_RIGHT}, {36, M_RIGHT}, {36, M_DOWN}, /* mirrored L */
	{37, M_ROTATE}, {37, M_ROTATE}, {37, M_RIGHT}, {37, M_DOWN}, /* L */
	{38, M_ROTATE}, {38, M_ROTATE}, {38, M_RIGHT}, {38, M_RIGHT}, {38, M_RIGHT}, {38, M_DOWN}, /* T */
	{39, M_LEFT}, {39, M_LEFT}, {39, M_LEFT}, {39, M_LEFT}, {39, M_LEFT}, {39, M_LEFT}, {39, M_DOWN}, /* I */
	{40, M_LEFT}, {40, M_LEFT}, {40, M_LEFT}, {40, M_DOWN}, /* Z */
	{41, M_LEFT}, {41,M_DOWN}, /* S */
	{42, M_RIGHT}, {42, M_RIGHT}, {42, M_RIGHT}, {42, M_RIGHT}, {42, M_RIGHT}, {42, M_RIGHT}, {42, M_RIGHT}, {42, M_DOWN}, /* square */
	{43, M_ROTATE}, {43, M_RIGHT}, {43, M_DOWN}, /* mirrored L */
	{44, M_RIGHT}, {44, M_RIGHT}, {44, M_RIGHT}, {44, M_RIGHT}, {44, M_DOWN}, /* L */
	{45, M_LEFT}, {45, M_LEFT}, {45, M_LEFT}, {45, M_LEFT}, {45, M_DOWN}, /* T */
	{46, M_LEFT}, {46, M_DOWN}, /* I */
	{47, M_ROTATE}, {47, M_LEFT}, {47, M_LEFT}, {47, M_LEFT}, {47, M_LEFT}, {47, M_DOWN}, /* Z */
	{48, M_LEFT}, {48, M_LEFT}, {48, M_DOWN}, /* S */
	{49, M_RIGHT}, {49, M_RIGHT}, {49, M_RIGHT}, {49, M_RIGHT}, {49, M_RIGHT}, {49, M_DOWN}, /* square */
	{50, M_LEFT}, {50, M_LEFT}, {50, M_LEFT}, {50, M_LEFT}, {50, M_LEFT}, {50, M_DOWN}, /* mirrored L */
	{51, M_ROTATE}, {51, M_ROTATE}, {51, M_ROTATE}, {51, M_RIGHT}, {51, M_DOWN}, /* L */
	{52, M_ROTATE}, {52, M_LEFT}, {52, M_LEFT}, {52, M_LEFT}, {52, M_LEFT}, {52, M_DOWN}, /* T */
	{53, M_LEFT}, {53, M_LEFT}, {53, M_LEFT}, {53, M_LEFT}, {53, M_LEFT}, {53, M_LEFT}, {53, M_DOWN}, /* I */
	{54, M_ROTATE}, {54, M_DOWN}, /* Z */
	{55, M_RIGHT}, {55, M_DOWN}, /* S */
	{56, M_RIGHT}, {56, M_RIGHT}, {56, M_RIGHT}, {56, M_RIGHT}, {56, M_RIGHT}, {56, M_RIGHT}, {56, M_RIGHT}, {56, M_DOWN}, /* square */
	{57, M_ROTATE}, {57, M_ROTATE}, {57, M_ROTATE}, {57, M_RIGHT}, {57, M_RIGHT}, {57, M_RIGHT}, {57, M_DOWN}, /* mirrored L */
	{58, M_LEFT}, {58, M_LEFT}, {58, M_LEFT}, {58, M_LEFT}, {58, M_DOWN}, /* L */
	{59, M_LEFT}, {59, M_DOWN}, /* T */
	{60, M_RIGHT}, {60, M_RIGHT}, {60, M_RIGHT}, {60, M_RIGHT}, {60, M_RIGHT}, {60, M_DOWN}, /* I */
	{61, M_ROTATE}, {61, M_RIGHT}, {61, M_RIGHT}, {61, M_RIGHT}, {61, M_DOWN}, /* Z */
	{62, M_LEFT}, {62, M_LEFT}, {62, M_LEFT}, {62, M_DOWN}, /* S */
	{63, M_RIGHT}, {63, M_RIGHT}, {63, M_RIGHT}, {63, M_RIGHT}, {63, M_RIGHT}, {63, M_RIGHT}, {63, M_RIGHT}, {63, M_DOWN}, /* square */
	{64, M_ROTATE}, {64, M_RIGHT}, {64, M_RIGHT}, {64, M_RIGHT}, {64, M_DOWN}, /* mirrored L */
	{65, M_ROTATE}, {65, M_RIGHT}, {65, M_RIGHT}, {65, M_RIGHT}, {65, M_RIGHT}, {65, M_RIGHT}, {65, M_RIGHT}, {65, M_DOWN}, /* L */
	{66, M_ROTATE}, {66, M_LEFT}, {66, M_DOWN}, /* T */
	{67, M_RIGHT}, {67, M_DOWN}, /* I */
	{68, M_ROTATE}, {68, M_RIGHT}, {68, M_RIGHT}, {68, M_DOWN}, /* Z */
	{69, M_RIGHT}, {69, M_RIGHT}, {69, M_RIGHT}, {69, M_RIGHT}, {69, M_DOWN}, /* S */
	{70, M_DOWN}, /* square */
	{71, M_ROTATE}, {71, M_ROTATE}, {71, M_LEFT}, {71, M_LEFT}, {71, M_DOWN}, /* mirrored L */
	{72, M_ROTATE}, {72, M_ROTATE}, {72, M_ROTATE}, {72, M_RIGHT}, {72, M_RIGHT}, {72, M_RIGHT}, {72, M_RIGHT}, {72, M_RIGHT}, {72, M_RIGHT}, {72, M_DOWN}, /* L */
	{73, M_LEFT}, {73, M_LEFT}, {73, M_LEFT}, {73, M_LEFT}, {73, M_LEFT}, {73, M_DOWN}, /* T */
	{74, M_ROTATE}, {74, M_RIGHT}, {74, M_RIGHT}, {74, M_RIGHT}, {74, M_RIGHT}, {74, M_RIGHT}, {74, M_DOWN}, /* I */
	{75, M_ROTATE}, {75, M_LEFT}, {75, M_LEFT}, {75, M_LEFT}, {75, M_LEFT}, {75, M_DOWN}, /* Z */
	{76, M_LEFT}, {76, M_LEFT}, {76, M_LEFT}, {76, M_DOWN}, /* S */
	{77, M_DOWN}, /* square */
	{78, M_ROTATE}, {78, M_ROTATE}, {78, M_RIGHT}, {78, M_RIGHT}, {78, M_DOWN}, /* mirrored L */
	{79, M_LEFT}, {79, M_LEFT}, {79, M_LEFT}, {79, M_LEFT}, {79, M_LEFT}, {79, M_DOWN}, /* L */
	{80, M_ROTATE}, {80, M_ROTATE}, {80, M_ROTATE}, {80, M_RIGHT}, {80, M_RIGHT}, {80, M_RIGHT}, {80, M_RIGHT}, {80, M_DOWN}, /* T */
	{81, M_LEFT}, {81, M_LEFT}, {81, M_DOWN}, /* I */
	{82, M_ROTATE}, {82, M_RIGHT}, {82, M_RIGHT}, {82, M_RIGHT}, {82, M_RIGHT}, {82, M_RIGHT}, {82, M_RIGHT}, {82, M_DOWN}, /* Z */
	{83, M_RIGHT}, {83, M_DOWN}, /* S */
	{84, M_RIGHT}, {84, M_RIGHT}, {84, M_DOWN}, /* square */
	{85, M_ROTATE}, {85, M_RIGHT}, {85, M_RIGHT}, {85, M_RIGHT}, {85, M_RIGHT}, {85, M_RIGHT}, {85, M_RIGHT}, {85, M_DOWN}, /* mirrored L */
	{86, M_ROTATE}, {86, M_ROTATE}, {86, M_LEFT}, {86, M_LEFT}, {86, M_LEFT}, {86, M_LEFT}, {86, M_LEFT}, {86, M_DOWN}, /* L */
	{87, M_RIGHT}, {87, M_RIGHT}, {87, M_RIGHT}, {87, M_RIGHT}, {87, M_DOWN}, /* T */
	{88, M_LEFT}, {88, M_LEFT}, {88, M_LEFT}, {88, M_DOWN}, /* I */
	{89, M_ROTATE}, {89, M_RIGHT}, {89, M_RIGHT}, {89, M_RIGHT}, {89, M_RIGHT}, {89, M_DOWN}, /* Z */
	{90, M_RIGHT}, {90, M_RIGHT}, {90, M_RIGHT}, {90, M_RIGHT}, {90, M_RIGHT}, {90, M_DOWN}, /* S */
	{91, M_RIGHT}, {91, M_RIGHT}, {91, M_RIGHT}, {91, M_RIGHT}, {91, M_RIGHT}, {91, M_RIGHT}, {91, M_RIGHT}, {91, M_DOWN}, /* square */
	{92, M_DOWN}, /* mirrored L */
	{93, M_ROTATE}, {93, M_ROTATE}, {93, M_LEFT}, {93, M_LEFT}, {93, M_LEFT}, {93, M_LEFT}, {93, M_DOWN}, /* L */
	{94, M_ROTATE}, {94, M_LEFT}, {94, M_LEFT}, {94, M_DOWN}, /* T */
	{95, M_LEFT}, {95, M_LEFT}, {95, M_LEFT}, {95, M_LEFT}, {95, M_LEFT}, {95, M_LEFT}, {95, M_DOWN}, /* I */
	{96, M_ROTATE}, {96, M_DOWN}, /* Z */
	{97, M_RIGHT}, {97, M_RIGHT}, {97, M_DOWN}, /* S */
	{98, M_RIGHT}, {98, M_RIGHT}, {98, M_RIGHT}, {98, M_RIGHT}, {98, M_RIGHT}, {98, M_RIGHT}, {98, M_RIGHT}, {98, M_DOWN}, /* square */
	{99, M_RIGHT}, {99, M_RIGHT}, {99, M_RIGHT}, {99, M_RIGHT},{99, M_DOWN}, /* mirrored L */
	{100, M_ROTATE}, {100, M_ROTATE}, {100, M_LEFT}, {100, M_LEFT}, {100, M_LEFT}, {100, M_LEFT}, {100, M_LEFT}, {100, M_DOWN}, /* L */
	{101, M_ROTATE}, {101, M_ROTATE}, {101, M_ROTATE}, {101, M_LEFT}, {101, M_LEFT}, {101, M_LEFT}, {101, M_DOWN}, /* T */
	{102, M_RIGHT}, {102, M_RIGHT}, {102, M_RIGHT}, {102, M_RIGHT}, {102, M_RIGHT}, {102, M_DOWN}, /* I */
	{103, M_LEFT}, {103, M_LEFT}, {103, M_LEFT}, {103, M_DOWN}, /* Z */
	{104, M_RIGHT}, {104, M_RIGHT}, {104, M_DOWN}, /* S */
	{105, M_RIGHT}, {105, M_RIGHT}, {105, M_RIGHT}, {105, M_RIGHT}, {105, M_RIGHT}, {105, M_RIGHT}, {105, M_RIGHT}, {105, M_DOWN}, /* square */
	{106, M_ROTATE}, {106, M_ROTATE}, {106, M_ROTATE}, {106, M_LEFT}, {106, M_DOWN}, /* mirrored L */
	{107, M_DOWN}, /* L */
	{108, M_ROTATE}, {108, M_ROTATE}, {108, M_ROTATE}, {108, M_LEFT}, {108, M_LEFT}, {108, M_LEFT}, {108, M_LEFT}, {108, M_LEFT}, {108, M_DOWN}, /* T */
	{109, M_ROTATE}, {109, M_RIGHT}, {109, M_RIGHT}, {109, M_RIGHT}, {109, M_RIGHT}, {109, M_RIGHT}, {109, M_RIGHT}, {109, M_DOWN}, /* I */
	{110, M_RIGHT}, {110, M_DOWN}, /* Z */
	{111, M_LEFT}, {111, M_LEFT}, {111, M_DOWN}, /* S */
	{112, M_RIGHT}, {112, M_RIGHT}, {112, M_RIGHT}, {112, M_DOWN}, /* square */
	{113, M_RIGHT}, {113, M_RIGHT}, {113, M_RIGHT}, {113, M_RIGHT}, {113, M_RIGHT}, {113, M_DOWN}, /* mirroredL */
	{114, M_RIGHT}, {114, M_RIGHT}, {114, M_RIGHT}, {114, M_RIGHT}, {114, M_RIGHT}, {114, M_RIGHT}, {114, M_RIGHT}, {114, M_DOWN}, /* L */
	{115, M_LEFT}, {115, M_LEFT}, {115, M_LEFT}, {115, M_LEFT}, {115, M_LEFT}, {115, M_DOWN}, /* T */
	{116, M_LEFT}, {116, M_LEFT}, {116, M_LEFT}, {116, M_LEFT}, {116, M_DOWN}, /* I */
	{117, M_ROTATE}, {117, M_LEFT}, {117, M_LEFT}, {117, M_DOWN}, /* Z */
	{118, M_ROTATE}, {118, M_RIGHT}, {118, M_RIGHT}, {118, M_RIGHT}, {118, M_DOWN}, /* S */
	{119, M_RIGHT}, {119, M_RIGHT}, {119, M_DOWN}, /* square */
	{120, M_ROTATE}, {120, M_RIGHT}, {120, M_RIGHT}, {120, M_RIGHT}, {120, M_RIGHT}, {120, M_DOWN}, /* mirrored L */
	{121, M_ROTATE}, {121, M_ROTATE}, {121, M_RIGHT}, {121, M_RIGHT}, {121, M_RIGHT}, {121, M_RIGHT}, {121, M_RIGHT}, {121, M_RIGHT}, {121, M_RIGHT}, {121, M_DOWN}, /* L */
	{122, M_ROTATE}, {122, M_ROTATE}, {122, M_DOWN}, /* T */
	{123, M_ROTATE}, {123, M_LEFT}, {123, M_LEFT}, {123, M_DOWN}, /* I */
	{124, M_LEFT}, {124, M_LEFT}, {124, M_LEFT}, {124, M_LEFT}, {124, M_DOWN}, /* Z */
	{125, M_ROTATE}, {125, M_RIGHT}, {125, M_RIGHT}, {125, M_RIGHT}, {125, M_RIGHT}, {125, M_RIGHT}, {125, M_DOWN}, /* S */
	{126, M_RIGHT}, {126, M_RIGHT}, {126, M_RIGHT}, {126, M_DOWN}, /* square */
	{127, M_ROTATE}, {127, M_ROTATE}, {127, M_ROTATE}, {127, M_DOWN}, /* mirrored L */
	{128, M_RIGHT}, {128, M_DOWN}, /* L */
	{129, M_LEFT}, {129, M_LEFT}, {129, M_LEFT}, {129, M_LEFT}, {129, M_LEFT}, {129, M_DOWN}, /* T */
	{130, M_RIGHT}, {130, M_RIGHT}, {130, M_RIGHT}, {130, M_RIGHT}, {130, M_RIGHT}, {130, M_RIGHT}, {130, M_RIGHT}, {130, M_RIGHT}, {130, M_DOWN}, /* I */
	{131, M_ROTATE}, {131, M_LEFT}, {131, M_LEFT}, {131, M_LEFT}, {131, M_DOWN}, /* Z */
	{132, M_LEFT}, {132, M_LEFT}, {132, M_LEFT}, {132, M_LEFT}, {132, M_LEFT}, {132, M_DOWN}, /* S */
	{133, M_RIGHT}, {133, M_RIGHT}, {133, M_DOWN}, /* square */
	{134, M_ROTATE}, {134, M_ROTATE}, {134, M_RIGHT}, {134, M_RIGHT}, {134, M_RIGHT}, {134, M_RIGHT}, {134, M_RIGHT}, {134, M_RIGHT}, {134, M_DOWN}, /* mirrored L */
	{135, M_ROTATE}, {135, M_ROTATE}, {135, M_RIGHT}, {135, M_RIGHT}, {135, M_RIGHT}, {135, M_DOWN}, /* L */
	{136, M_ROTATE}, {136, M_LEFT}, {136, M_LEFT}, {136, M_DOWN}, /* T */
	{137, M_RIGHT}, {137, M_RIGHT}, {137, M_RIGHT}, {137, M_RIGHT}, {137, M_DOWN}, /* I */
	{138, M_LEFT}, {138, M_LEFT}, {138, M_LEFT}, {138, M_DOWN}, /* Z */
	{139, M_RIGHT}, {139, M_RIGHT}, {139, M_RIGHT}, {139, M_RIGHT}, {139, M_RIGHT}, {139, M_DOWN}, /* S */
	{140, M_RIGHT}, {140, M_RIGHT}, {140, M_RIGHT}, {140, M_RIGHT}, {140, M_RIGHT}, {140, M_RIGHT}, {140, M_DOWN}, /* square */
	{141, M_ROTATE}, {141, M_ROTATE}, {141, M_LEFT}, {141, M_DOWN}, /* mirrored L */
	{142, M_RIGHT}, {142, M_DOWN}, /* L */
	{143, M_ROTATE}, {143, M_LEFT}, {143, M_LEFT}, {143, M_LEFT}, {143, M_LEFT}, {143, M_LEFT}, {143, M_DOWN}, /* T */
	{144, M_RIGHT}, {144, M_RIGHT}, {144, M_RIGHT}, {144, M_RIGHT}, {144, M_RIGHT}, {144, M_RIGHT}, {144, M_RIGHT}, {144, M_DOWN}, /* I */
	{145, M_RIGHT}, {145, M_RIGHT}, {145, M_RIGHT}, {145, M_RIGHT}, {145, M_DOWN}, /* Z */
	{146, M_LEFT}, {146, M_LEFT}, {146, M_DOWN}, /* S */
	{147, M_RIGHT}, {147, M_RIGHT}, {147, M_DOWN}, /* square */
	{148, M_LEFT}, {148, M_LEFT}, {148, M_LEFT}, {148, M_DOWN}, /* mirrored L */
	{149, M_LEFT}, {149, M_LEFT}, {149, M_LEFT}, {149, M_LEFT}, {149, M_LEFT}, {149, M_DOWN}, /* L */
	{150, M_DOWN}, /* T */
	{151, M_LEFT}, {151, M_LEFT}, {151, M_DOWN}, /* I */
	{152, M_ROTATE}, {152, M_RIGHT}, {152, M_DOWN}, /* Z */
	{153, M_RIGHT}, {153, M_RIGHT}, {153, M_RIGHT}, {153, M_DOWN}, /* S */
	{154, M_LEFT}, {154, M_DOWN}, /* square */
	{155, M_LEFT}, {155, M_LEFT}, {155, M_LEFT}, {155, M_LEFT}, {155, M_DOWN}, /* mirrored L */
	{156, M_ROTATE}, {156, M_LEFT}, {156, M_LEFT}, {156, M_LEFT}, {156, M_LEFT}, {156, M_LEFT}, {156, M_DOWN}, /* L */
	{157, M_ROTATE}, {157, M_ROTATE}, {157, M_RIGHT}, {157, M_DOWN}, /* T */
	/*{158, M_INIT},*/ /* RESTART */
	{158, M_LEFT}, {158, M_LEFT}, {158, M_LEFT}, {158, M_LEFT}, {158, M_LEFT}, {158, M_LEFT}, {158, M_DOWN}, /* I */
	{159, M_ROTATE}, {159, M_RIGHT}, {159, M_RIGHT}, {159, M_RIGHT}, {159, M_RIGHT}, {159, M_RIGHT}, {159, M_DOWN}, /* Z */
	{160, M_INIT},
	{160, M_RIGHT}, {160, M_RIGHT}, {160, M_RIGHT}, {160, M_RIGHT}, {160, M_RIGHT}, {160, M_RIGHT}, {160, M_RIGHT}, {160, M_DOWN}, /* S */
	{161, M_LEFT}, {161, M_LEFT}, {161, M_LEFT}, {161, M_DOWN}, /* square */
	{162, M_RIGHT}, {162, M_RIGHT}, {162, M_RIGHT}, {162, M_RIGHT}, {162, M_DOWN}, /* mirrored L */
	{163, M_LEFT}, {163, M_DOWN}, /* L */
	{164, M_ROTATE}, {164, M_DOWN}, {164, M_DOWN}, /* T */
	{165, M_RIGHT}, {165, M_RIGHT}, {165, M_DOWN}, /* I */
	{166, M_ROTATE}, {166,M_LEFT}, {166,M_LEFT}, {166,M_LEFT}, {166,M_LEFT}, {166,M_LEFT}, {166, M_DOWN}, /* Z */
	{167, M_RIGHT}, {167, M_RIGHT}, {167, M_RIGHT}, {167, M_RIGHT}, {167, M_RIGHT}, {167, M_RIGHT}, {167, M_RIGHT}, {167, M_DOWN}, /* S */
	{168, M_RIGHT}, {168, M_DOWN}, /* square */
	{169, M_RIGHT}, {169, M_RIGHT}, {169, M_RIGHT}, {169, M_RIGHT}, {169, M_ROTATE}, {169, M_DOWN}, /* mirrored L */
	{170, M_INIT},
	{171, 0}, {172, M_DOWN},
	{172, 0}, {172, M_DOWN},
	{173, 0}, {173, M_DOWN},
	{174, 0}, {174, M_DOWN},
	{175, 0}, {175, M_DOWN},
	{176, 0}, {176, M_DOWN},
	{177, 0}, {177, M_DOWN},
	{178, 0}, {178, M_DOWN},
	{179, 0}, {179, M_DOWN},
	{255, M_INIT} };

TASK(tsiminnk)
{
	static unsigned int	idx;
	TaskStateType		state;

	if (GetTaskState(tsiminrx, &state) == E_OK && state == WAITING)
	{
		if (ai[idx].time <= no_pieces)
		{
			if (ai[idx].key == 0)
			{
				ai[idx].time = ai[idx].key;
			}
			next_char = ai[idx].key;
			idx++;
			SetEvent(tsiminrx, esiminnk);
		}
	}

	if (idx == 0 || ai[idx-1].key != M_INIT)
	{
		SetRelAlarm(asiminnk, 10 - no_pieces / 20, 0);
	}
	else
	{
		idx = 0;
	}

	TerminateTask();
}
#endif /* TETRIS */

#ifdef DEMO
static int no_seconds;

ALARMCALLBACK(cbsiminns)
{
	no_seconds ++;
}

static struct keys ai[] = {
	{1, 'n'}, {1, 'o'}, {1, 'p'}, {1, 'r'}, {1, 'i'}, {1, 'n'}, {1, 't'}, {1, 'a'}, {1, '\n'},
	{2, 'n'}, {2, 'o'}, {2, 'p'}, {2, 'r'}, {2, 'i'}, {2, 'n'}, {2, 't'}, {2, 'b'}, {2, '\n'},
	{3, 'n'}, {3, 'o'}, {3, 'p'}, {3, 'r'}, {3, 'i'}, {3, 'n'}, {3, 't'}, {3, 'c'}, {3, '\n'},
	{4, 'c'}, {4, 'h'}, {4, 'a'}, {4, 'n'}, {4, 'g'}, {4, 'e'}, {4, '\n'}, 
	{5, 'h'}, {5, 'e'}, {5, 'l'}, {5, 'p'}, {5, '\n'},
	{6, 's'}, {6, 't'}, {6, 'a'}, {6, 'r'}, {6, 't'}, {6, 'a'}, {6, '\n'},
	{9, 's'}, {9, 't'}, {9, 'o'}, {9, 'p'}, {9, 'a'}, {9, '\n'},
	{12, 's'}, {12, 't'}, {12, 'a'}, {12, 'r'}, {12, 't'}, {12, 'a'}, {12, '\n'},
	{14, 'n'}, {14, 'o'}, {14, 'p'}, {14, 'r'}, {14, 'i'}, {14, 'n'}, {14, 't'}, {14, 'a'}, {14, '\n'},
	{15, 's'}, {15, 't'}, {15, 'o'}, {15, 'p'}, {15, 'a'}, {15, '\n'},
	{18, 's'}, {18, 't'}, {18, 'a'}, {18, 'r'}, {18, 't'}, {18, 'a'}, {18, '\n'},
	{19, 's'}, {19, 't'}, {19, 'a'}, {19, 'r'}, {19, 't'}, {19, 'b'}, {19, '\n'},
	{20, 's'}, {20, 't'}, {20, 'a'}, {20, 'r'}, {20, 't'}, {20, 'c'}, {20, '\n'},
	{25, 'e'}, {25, 'n'}, {25, 'd'}, {25, 'c'}, {18, '\n'},
	{25, 'e'}, {25, 'n'}, {25, 'd'}, {25, 'b'}, {18, '\n'},
	{25, 'e'}, {25, 'n'}, {25, 'd'}, {25, 'a'}, {18, '\n'},
	{28, 'c'}, {28, 'h'}, {28, 'a'}, {28, 'n'}, {28, 'g'}, {28, 'e'}, {28, '\n'}, 
	{40, 'c'}, {40, 'h'}, {40, 'a'}, {40, 'n'}, {40, 'g'}, {40, 'e'}, {40, '\n'}, 
	{255, 'h'}, {255, 'e'}, {255, 'l'}, {255, 'p'}, {255, '\n'},
	{0, 0} };

TASK(tsiminnk)
{
	static unsigned int	idx;
	TaskStateType		state;
	char			buf[2];

	if (GetTaskState(tsiminrx, &state) == E_OK && state == WAITING)
	{
		if (ai[idx].time <= no_seconds)
		{
			if (ai[idx].key == 0)
			{
				ai[idx].time = ai[idx].key;
			}
			next_char = ai[idx].key;
			idx++;
			buf[0] = next_char;
			buf[1] = '\0';
			outs(SHELLOUT, buf);
			outflush(SHELLOUT);
			SetEvent(tsiminrx, esiminnk);
		}
	}

	if (idx == 0 || ai[idx-1].key != 0)
	{
		SetRelAlarm(asiminnk, 5, 0);
	}
	else
	{
		idx = 0;
	}

	TerminateTask();
}
#endif /* DEMO */


TASK(tsiminrx)
{
	ioreq		*preqrx;

	while(1)
	{
		/* wake up when someone wants a character */
		WaitEvent(esiminrx);

		/* clear it after we have waited for it if we clear if just before we wait for it
		 * some one might already have set the event to wake us up
		 */
		ClearEvent(esiminrx);

		if (E_OK != ReceiveMessage(mrsiminrx, &preqrx))
		{
			/* an io request has been lost */
			ShutdownOS(E_ERR_IO_OFLW);
		}
		else if (preqrx->len != 1)	/* line requests */
		{
			do
			{
				ClearEvent(esiminnk);
				WaitEvent(esiminnk);
				preqrx->buf[preqrx->pos++] = next_char;
			} while (next_char != '\n' && preqrx->pos != preqrx->len-1);
			preqrx->buf[preqrx->pos-1] = '\0';
		}
		else				/* single character requests */
		{
			ClearEvent(esiminnk);
			WaitEvent(esiminnk);
			preqrx->buf[preqrx->pos] = next_char;
		}

		/* wake up read task */
		SetEvent(preqrx->task, preqrx->event);
	}

	TerminateTask();
}

#endif /* SIMINRX */

void simin_init(void)
{
	/* init the simin device */
	devices[SIMIN].txevent	= 0;
	devices[SIMIN].txmsg	= -1;
#if defined(SIMINRX)
	devices[SIMIN].rxevent	= esiminrx;
	devices[SIMIN].rxmsg	= mssiminrx;
#else
	devices[SIMIN].rxevent	= 0;
	devices[SIMIN].rxmsg	= -1;
#endif
}

