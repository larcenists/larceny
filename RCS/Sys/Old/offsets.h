/* -*- Fundamental -*-
 * Scheme Run-Time System.
 *
 * Offsets into the "globals" table, with definitions for both C and
 * Assembly language. Define the symbol "ASSEMBLY" to get the assembly
 * language definitions.
 *
 * The roots must be contiguous and between FIRST_ROOT and LAST_ROOT,
 * inclusive.
 *
 * $Id: offsets.h,v 1.8 91/07/24 12:13:44 lth Exp Locker: lth $
 */

#ifdef ASSEMBLY
#define ADJUST		4
#else
#define ADJUST		1
#endif

/* Be careful with these! */
#define FIRST_ROOT		20
#define LAST_ROOT		58
#define LAST_GLOBAL             86

/* Table size */
#define GLOBALS_TABLE_SIZE	(LAST_GLOBAL + 1)

/* Offsets. No trailing comments, please! */

/* Nonroots */
#define T_BASE_OFFSET		(0*ADJUST)
#define T_TOP_OFFSET		(1*ADJUST)
#define T_TRANS_OFFSET		(2*ADJUST)
#define T_MAX_OFFSET		(3*ADJUST)
#define E_BASE_OFFSET		(4*ADJUST)
#define E_TOP_OFFSET		(5*ADJUST)
#define E_MARK_OFFSET		(6*ADJUST)
#define E_LIMIT_OFFSET		(7*ADJUST)
#define E_MAX_OFFSET		(8*ADJUST)
#define S_BASE_OFFSET		(9*ADJUST)
#define S_MAX_OFFSET		(10*ADJUST)
#define STK_BASE_OFFSET		(11*ADJUST)
#define STK_LIMIT_OFFSET	(12*ADJUST)
#define STK_START_OFFSET        (13*ADJUST)
#define STK_MAX_OFFSET		(14*ADJUST)
#define E_COLLECTIONS_OFFSET	(15*ADJUST)
#define T_COLLECTIONS_OFFSET	(16*ADJUST)
#define WCOLLECTED_OFFSET	(17*ADJUST)
#define WALLOCATED_OFFSET	(18*ADJUST)
#define INITIAL_TIMER_OFFSET    (19*ADJUST)

/* Roots */
#define REG0_OFFSET		(20*ADJUST)
#define REG1_OFFSET		(21*ADJUST)
#define REG2_OFFSET		(22*ADJUST)
#define REG3_OFFSET		(23*ADJUST)
#define REG4_OFFSET		(24*ADJUST)
#define REG5_OFFSET		(25*ADJUST)
#define REG6_OFFSET		(26*ADJUST)
#define REG7_OFFSET		(27*ADJUST)
#define REG8_OFFSET		(28*ADJUST)
#define REG9_OFFSET		(29*ADJUST)
#define REG10_OFFSET		(30*ADJUST)
#define REG11_OFFSET		(31*ADJUST)
#define REG12_OFFSET		(32*ADJUST)
#define REG13_OFFSET		(33*ADJUST)
#define REG14_OFFSET		(34*ADJUST)
#define REG15_OFFSET		(35*ADJUST)
#define REG16_OFFSET		(36*ADJUST)
#define REG17_OFFSET		(37*ADJUST)
#define REG18_OFFSET		(38*ADJUST)
#define REG19_OFFSET		(39*ADJUST)
#define REG20_OFFSET		(40*ADJUST)
#define REG21_OFFSET		(41*ADJUST)
#define REG22_OFFSET		(42*ADJUST)
#define REG23_OFFSET		(43*ADJUST)
#define REG24_OFFSET		(44*ADJUST)
#define REG25_OFFSET		(45*ADJUST)
#define REG26_OFFSET		(46*ADJUST)
#define REG27_OFFSET		(47*ADJUST)
#define REG28_OFFSET		(48*ADJUST)
#define REG29_OFFSET		(49*ADJUST)
#define REG30_OFFSET		(50*ADJUST)
#define REG31_OFFSET		(51*ADJUST)
#define ARGREG2_OFFSET		(52*ADJUST)
#define ARGREG3_OFFSET		(53*ADJUST)
#define RESULT_OFFSET		(54*ADJUST)
#define CONTINUATION_OFFSET	(55*ADJUST)
/* Roots which are millicode temporaries */
#define SAVED_RESULT_OFFSET	(56*ADJUST)
/* More roots */
#define SCHEME_ENTRY_OFFSET     (57*ADJUST)
#define SYMTAB_OFFSET           (58*ADJUST)

/* More nonroots */
#define LOMEM_OFFSET            (70*ADJUST)
#define HIMEM_OFFSET            (71*ADJUST)
#define SP_OFFSET		(72*ADJUST)
#define TIMER_OFFSET		(73*ADJUST)
#define E_NEW_BASE_OFFSET	(74*ADJUST)
#define E_NEW_MAX_OFFSET	(75*ADJUST)
#define T_NEW_BASE_OFFSET	(76*ADJUST)
#define T_NEW_MAX_OFFSET	(77*ADJUST)
#define MUST_TENURE_OFFSET	(78*ADJUST)
#define F_COLLECTIONS_OFFSET	(79*ADJUST)
#define WCOPIED_OFFSET		(80*ADJUST)
#define SAVED_F2_OFFSET         (81*ADJUST)
#define SAVED_F3_OFFSET         (82*ADJUST)
#define SAVED_F4_OFFSET         (83*ADJUST)
#define SAVED_F5_OFFSET         (84*ADJUST)
#define SAVED_RETADDR_OFFSET    (85*ADJUST)
#define ARITH_SAVED_RETADDR_OFFSET  (86*ADJUST)
