/*
 * Offsets into the "globals" table, with definitions for both C and
 * Assembly language. Define the symbol "ASSEMBLY" to get the assembly
 * language definitions.
 *
 * $Id$
 */

#ifndef ASSEMBLY

/* For C language */

/* To define/declare table somewhere */

#define GLOBALS_TABLE_SIZE      LAST_GLOBAL

/* These must reflect reality! */

#define FIRST_ROOT		5
#define LAST_ROOT		16

#define T_BASE_OFFSET		0
#define T_ENTRIES_OFFSET	1
#define T_TOP_OFFSET		2
#define E_TOP_OFFSET		3
#define E_LIMIT_OFFSET		4
#define REG0_OFFSET		5
#define REG1_OFFSET		6
#define REG2_OFFSET		7
#define REG3_OFFSET		8
#define REG4_OFFSET		9
#define REG5_OFFSET		10
#define REG6_OFFSET		11
#define REG7_OFFSET		12
#define ARGREG2_OFFSET		13
#define ARGREG3_OFFSET		14
#define RESULT_OFFSET		15
#define CONTINUATION_OFFSET	16

/* Make sure you update this one! */

#define LAST_GLOBAL             17

#else

/* For assembly language */

#define T_BASE_OFFSET		0
#define T_ENTRIES_OFFSET	4
#define T_TOP_OFFSET		8
#define E_TOP_OFFSET		12
#define E_LIMIT_OFFSET		16
#define REG0_OFFSET		20
#define REG1_OFFSET		24
#define REG2_OFFSET		28
#define REG3_OFFSET		32
#define REG4_OFFSET		36
#define REG5_OFFSET		40
#define REG6_OFFSET		44
#define REG7_OFFSET		48
#define ARGREG2_OFFSET		52
#define ARGREG3_OFFSET		56
#define RESULT_OFFSET		60
#define CONTINUATION_OFFSET	64

#endif
