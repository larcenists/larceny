/*
 * Offsets into the "globals" table, with definitions for both C and
 * Assembly language. Define the symbol "ASSEMBLY" to get the assembly
 * language definitions.
 *
 * $Id: offsets.h,v 1.1 91/06/20 15:47:56 lth Exp Locker: lth $
 */

#ifndef ASSEMBLY

/* For C language */

/* To define/declare table somewhere */

#define GLOBALS_TABLE_SIZE      LAST_GLOBAL

/* These must reflect reality! */

#define FIRST_ROOT		20
#define LAST_ROOT		31

#define T_BASE_OFFSET		0
#define T_TOP_OFFSET		1
#define T_TRANS_OFFSET		2
#define T_MAX_OFFSET		3
#define E_BASE_OFFSET		4
#define E_TOP_OFFSET		5
#define E_MARK_OFFSET		6
#define E_LIMIT_OFFSET		7
#define E_MAX_OFFSET		8
#define S_BASE_OFFSET		9
#define S_MAX_OFFSET		10
#define STK_BASE_OFFSET		11
#define STK_LIMIT_OFFSET	12
#define STK_MAX_OFFSET		13
#define ECOLLECTIONS_OFFSET	14
#define TCOLLECTIONS_OFFSET	15
#define WCOLLECTED_OFFSET	16
#define WALLOCATED_OFFSET	17
#define INITIAL_TIMER_OFFSET    18

#define REG0_OFFSET		20
#define REG1_OFFSET		21
#define REG2_OFFSET		22
#define REG3_OFFSET		23
#define REG4_OFFSET		24
#define REG5_OFFSET		25
#define REG6_OFFSET		26
#define REG7_OFFSET		27
#define ARGREG2_OFFSET		28
#define ARGREG3_OFFSET		29
#define RESULT_OFFSET		30
#define CONTINUATION_OFFSET	31

#define LOMEM_OFFSET            50
#define HIMEM_OFFSET            51

/* Make sure you update this one! */

#define LAST_GLOBAL             51

#else

/* For assembly language */

#define T_BASE_OFFSET		0
#define T_TOP_OFFSET		4
#define T_TRANS_OFFSET		8
#define T_MAX_OFFSET		12
#define E_BASE_OFFSET		16
#define E_TOP_OFFSET		20
#define E_MARK_OFFSET		24
#define E_LIMIT_OFFSET		28
#define E_MAX_OFFSET		32
#define S_BASE_OFFSET		36
#define S_MAX_OFFSET		40
#define STK_BASE_OFFSET		44
#define STK_LIMIT_OFFSET	48
#define STK_MAX_OFFSET		52
#define ECOLLECTIONS_OFFSET	56
#define TCOLLECTIONS_OFFSET	60
#define WCOLLECTED_OFFSET	64
#define WALLOCATED_OFFSET	68
#define INITIAL_TIMER_OFFSET    72

#define REG0_OFFSET		80
#define REG1_OFFSET		84
#define REG2_OFFSET		88
#define REG3_OFFSET		92
#define REG4_OFFSET		96
#define REG5_OFFSET		100
#define REG6_OFFSET		104
#define REG7_OFFSET		108
#define ARGREG2_OFFSET		112
#define ARGREG3_OFFSET		116
#define RESULT_OFFSET		120
#define CONTINUATION_OFFSET	124

#define LOMEM_OFFSET            200
#define HIMEM_OFFSET            204

#endif
