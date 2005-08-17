using System;
using Scheme.Rep;

namespace Scheme.RT {

public sealed class Instructions
{

  /* ===================================================== */
  /*   Registers, Constants, and Top-level Environment     */
  /* ===================================================== */

  /** nop */
  public static void nop()
  {
    return;
  }

  /** global
   * Retrieves global variable at the given index from the current
   * Procedure's global array. Uses given string name to report error.
   */
  public static void global (int index, string name)
  {
    Procedure thisProc = Reg.Register0;
    // This way of doing it does dynamic casting, which is very slow.
    //
    //        SPair cell = (SPair)thisProc.constants[index];
    //
    //        if (cell.first == Factory.Undefined) {
    //            Exn.fault(Constants.EX_UNDEF_GLOBAL,
    //                      "reference to unbound global: " + name,
    //                      cell);
    //            return;
    //        } else {
    //            Reg.Result = cell.first;
    //        }

    SObject value = thisProc.constants[index].op_cell_ref();
    if (value == Factory.Undefined) {
	Exn.fault (Constants.EX_UNDEF_GLOBAL,
		   "reference to unbound global: " + name,
		   thisProc.constants[index]);
	return;
	}
    else {
	Reg.Result = value;
	}
  }

  /** setglbl
   * Set global variable at given index in current Procedure's global
   * array.
   */
  public static void setglbl (int index)
  {
    Reg.Register0.constants[index].op_cell_set (Reg.Result);
  }

  /** constant
   */
  public static void constant (int constIndex)
  {
    Reg.Result = Reg.Register0.constants[constIndex];
  }

  // These aren't worth it for speed, but they do make the code smaller.
  public static void constant1 () { Reg.Result = Reg.Register0.constants [1]; }
  public static void constant2 () { Reg.Result = Reg.Register0.constants [2]; }
  public static void constant3 () { Reg.Result = Reg.Register0.constants [3]; }
  public static void constant4 () { Reg.Result = Reg.Register0.constants [4]; }
  public static void constant5 () { Reg.Result = Reg.Register0.constants [5]; }
  public static void constant6 () { Reg.Result = Reg.Register0.constants [6]; }
  public static void constant7 () { Reg.Result = Reg.Register0.constants [7]; }
  public static void constant8 () { Reg.Result = Reg.Register0.constants [8]; }

  /** imm_constant
   * Loads Reg.Result with immediate constant
   */
  public static void imm_constant (SObject immediate)
  {
    Reg.Result = immediate;
  }

  /** imm_const_setreg
   * Loads given register with immediate constant
   */
  public static void imm_const_setreg (SObject immediate, int register)
  {
    Reg.setRegister (register, immediate);
  }

  /** reg
   * Load Register k into Reg.Result
   */
  public static void reg (int k)
  {
    Reg.Result = Reg.getRegister (k);
  }

  // These are important.
  public static void reg0 ()  { Reg.Result = Reg.Register0; }
  public static void reg1 ()  { Reg.Result = Reg.Register1; }
  public static void reg2 ()  { Reg.Result = Reg.Register2; }
  public static void reg3 ()  { Reg.Result = Reg.Register3; }
  public static void reg4 ()  { Reg.Result = Reg.Register4; }
  public static void reg5 ()  { Reg.Result = Reg.Register5; }
  public static void reg6 ()  { Reg.Result = Reg.Register6; }
  public static void reg7 ()  { Reg.Result = Reg.Register7; }
  public static void reg8 ()  { Reg.Result = Reg.Register8; }
  public static void reg9 ()  { Reg.Result = Reg.Register9; }
  public static void reg10 () { Reg.Result = Reg.Register10; }
  public static void reg11 () { Reg.Result = Reg.Register11; }
  public static void reg12 () { Reg.Result = Reg.Register12; }
  public static void reg13 () { Reg.Result = Reg.Register13; }
  public static void reg14 () { Reg.Result = Reg.Register14; }
  public static void reg15 () { Reg.Result = Reg.Register15; }
  public static void reg16 () { Reg.Result = Reg.Register16; }
  public static void reg17 () { Reg.Result = Reg.Register17; }
  public static void reg18 () { Reg.Result = Reg.Register18; }
  public static void reg19 () { Reg.Result = Reg.Register19; }
  public static void reg20 () { Reg.Result = Reg.Register20; }
  public static void reg21 () { Reg.Result = Reg.Register21; }
  public static void reg22 () { Reg.Result = Reg.Register22; }
  public static void reg23 () { Reg.Result = Reg.Register23; }
  public static void reg24 () { Reg.Result = Reg.Register24; }
  public static void reg25 () { Reg.Result = Reg.Register25; }
  public static void reg26 () { Reg.Result = Reg.Register26; }
  public static void reg27 () { Reg.Result = Reg.Register27; }
  public static void reg28 () { Reg.Result = Reg.Register28; }
  public static void reg29 () { Reg.Result = Reg.Register29; }
  public static void reg30 () { Reg.Result = Reg.Register30; }
  public static void reg31 () { Reg.Result = Reg.Register31; }

  /** setreg
   * Load Reg.Result into Register k
   */
  public static void setreg (int k)
  {
    Reg.setRegister (k, Reg.Result);
  }

  public static void setreg0 ()  { Reg.Register0 = (Procedure) Reg.Result; }
  public static void setreg1 ()  { Reg.Register1 = Reg.Result; }
  public static void setreg2 ()  { Reg.Register2 = Reg.Result; }
  public static void setreg3 ()  { Reg.Register3 = Reg.Result; }
  public static void setreg4 ()  { Reg.Register4 = Reg.Result; }
  public static void setreg5 ()  { Reg.Register5 = Reg.Result; }
  public static void setreg6 ()  { Reg.Register6 = Reg.Result; }
  public static void setreg7 ()  { Reg.Register7 = Reg.Result; }
  public static void setreg8 ()  { Reg.Register8 = Reg.Result; }
  public static void setreg9 ()  { Reg.Register9 = Reg.Result; }
  public static void setreg10 () { Reg.Register10 = Reg.Result; }
  public static void setreg11 () { Reg.Register11 = Reg.Result; }
  public static void setreg12 () { Reg.Register12 = Reg.Result; }
  public static void setreg13 () { Reg.Register13 = Reg.Result; }
  public static void setreg14 () { Reg.Register14 = Reg.Result; }
  public static void setreg15 () { Reg.Register15 = Reg.Result; }
  public static void setreg16 () { Reg.Register16 = Reg.Result; }
  public static void setreg17 () { Reg.Register17 = Reg.Result; }
  public static void setreg18 () { Reg.Register18 = Reg.Result; }
  public static void setreg19 () { Reg.Register19 = Reg.Result; }
  public static void setreg20 () { Reg.Register20 = Reg.Result; }
  public static void setreg21 () { Reg.Register21 = Reg.Result; }
  public static void setreg22 () { Reg.Register22 = Reg.Result; }
  public static void setreg23 () { Reg.Register23 = Reg.Result; }
  public static void setreg24 () { Reg.Register24 = Reg.Result; }
  public static void setreg25 () { Reg.Register25 = Reg.Result; }
  public static void setreg26 () { Reg.Register26 = Reg.Result; }
  public static void setreg27 () { Reg.Register27 = Reg.Result; }
  public static void setreg28 () { Reg.Register28 = Reg.Result; }
  public static void setreg29 () { Reg.Register29 = Reg.Result; }
  public static void setreg30 () { Reg.Register30 = Reg.Result; }
  public static void setreg31 () { Reg.Register31 = Reg.Result; }

  /** movereg
   * Move Register src to Register dst
   */
  public static void movereg (int src, int dst)
  {
    Reg.setRegister (dst, Reg.getRegister (src));
  }

  public static void movereg_0_1 () { Reg.Register1 = Reg.Register0; }
  public static void movereg_1_2 () { Reg.Register2 = Reg.Register1; }
  public static void movereg_1_3 () { Reg.Register3 = Reg.Register1; }
  public static void movereg_1_4 () { Reg.Register4 = Reg.Register1; }
  public static void movereg_1_5 () { Reg.Register5 = Reg.Register1; }
  public static void movereg_1_6 () { Reg.Register6 = Reg.Register1; }
  public static void movereg_1_7 () { Reg.Register7 = Reg.Register1; }
  public static void movereg_1_8 () { Reg.Register8 = Reg.Register1; }
  public static void movereg_1_9 () { Reg.Register9 = Reg.Register1; }
  public static void movereg_1_10 () { Reg.Register10 = Reg.Register1; }
  public static void movereg_1_11 () { Reg.Register11 = Reg.Register1; }
  public static void movereg_1_12 () { Reg.Register12 = Reg.Register1; }
  public static void movereg_1_13 () { Reg.Register13 = Reg.Register1; }
  public static void movereg_1_14 () { Reg.Register14 = Reg.Register1; }
  public static void movereg_1_15 () { Reg.Register15 = Reg.Register1; }
  public static void movereg_1_16 () { Reg.Register16 = Reg.Register1; }
  public static void movereg_1_17 () { Reg.Register17 = Reg.Register1; }
  public static void movereg_1_18 () { Reg.Register18 = Reg.Register1; }
  public static void movereg_1_19 () { Reg.Register19 = Reg.Register1; }
  public static void movereg_1_20 () { Reg.Register20 = Reg.Register1; }
  public static void movereg_1_21 () { Reg.Register21 = Reg.Register1; }
  public static void movereg_1_22 () { Reg.Register22 = Reg.Register1; }
  public static void movereg_1_23 () { Reg.Register23 = Reg.Register1; }
  public static void movereg_1_24 () { Reg.Register24 = Reg.Register1; }
  public static void movereg_1_25 () { Reg.Register25 = Reg.Register1; }
  public static void movereg_1_26 () { Reg.Register26 = Reg.Register1; }
  public static void movereg_1_27 () { Reg.Register27 = Reg.Register1; }
  public static void movereg_1_28 () { Reg.Register28 = Reg.Register1; }
  public static void movereg_1_29 () { Reg.Register29 = Reg.Register1; }
  public static void movereg_1_30 () { Reg.Register30 = Reg.Register1; }
  public static void movereg_1_31 () { Reg.Register31 = Reg.Register1; }
  public static void movereg_2_1 () { Reg.Register1 = Reg.Register2; }
  public static void movereg_2_3 () { Reg.Register3 = Reg.Register2; }
  public static void movereg_2_4 () { Reg.Register4 = Reg.Register2; }
  public static void movereg_2_5 () { Reg.Register5 = Reg.Register2; }
  public static void movereg_2_6 () { Reg.Register6 = Reg.Register2; }
  public static void movereg_2_7 () { Reg.Register7 = Reg.Register2; }
  public static void movereg_2_8 () { Reg.Register8 = Reg.Register2; }
  public static void movereg_2_9 () { Reg.Register9 = Reg.Register2; }
  public static void movereg_2_10 () { Reg.Register10 = Reg.Register2; }
  public static void movereg_2_11 () { Reg.Register11 = Reg.Register2; }
  public static void movereg_2_12 () { Reg.Register12 = Reg.Register2; }
  public static void movereg_2_13 () { Reg.Register13 = Reg.Register2; }
  public static void movereg_2_14 () { Reg.Register14 = Reg.Register2; }
  public static void movereg_2_15 () { Reg.Register15 = Reg.Register2; }
  public static void movereg_2_16 () { Reg.Register16 = Reg.Register2; }
  public static void movereg_2_17 () { Reg.Register17 = Reg.Register2; }
  public static void movereg_2_18 () { Reg.Register18 = Reg.Register2; }
  public static void movereg_2_19 () { Reg.Register19 = Reg.Register2; }
  public static void movereg_2_20 () { Reg.Register20 = Reg.Register2; }
  public static void movereg_2_21 () { Reg.Register21 = Reg.Register2; }
  public static void movereg_2_22 () { Reg.Register22 = Reg.Register2; }
  public static void movereg_2_23 () { Reg.Register23 = Reg.Register2; }
  public static void movereg_2_24 () { Reg.Register24 = Reg.Register2; }
  public static void movereg_2_25 () { Reg.Register25 = Reg.Register2; }
  public static void movereg_2_26 () { Reg.Register26 = Reg.Register2; }
  public static void movereg_2_27 () { Reg.Register27 = Reg.Register2; }
  public static void movereg_2_28 () { Reg.Register28 = Reg.Register2; }
  public static void movereg_2_29 () { Reg.Register29 = Reg.Register2; }
  public static void movereg_2_30 () { Reg.Register30 = Reg.Register2; }
  public static void movereg_2_31 () { Reg.Register31 = Reg.Register2; }
  public static void movereg_3_1 () { Reg.Register1 = Reg.Register3; }
  public static void movereg_3_2 () { Reg.Register2 = Reg.Register3; }
  public static void movereg_3_4 () { Reg.Register4 = Reg.Register3; }
  public static void movereg_3_5 () { Reg.Register5 = Reg.Register3; }
  public static void movereg_3_6 () { Reg.Register6 = Reg.Register3; }
  public static void movereg_3_7 () { Reg.Register7 = Reg.Register3; }
  public static void movereg_3_8 () { Reg.Register8 = Reg.Register3; }
  public static void movereg_3_9 () { Reg.Register9 = Reg.Register3; }
  public static void movereg_3_10 () { Reg.Register10 = Reg.Register3; }
  public static void movereg_3_11 () { Reg.Register11 = Reg.Register3; }
  public static void movereg_3_12 () { Reg.Register12 = Reg.Register3; }
  public static void movereg_3_13 () { Reg.Register13 = Reg.Register3; }
  public static void movereg_3_14 () { Reg.Register14 = Reg.Register3; }
  public static void movereg_3_15 () { Reg.Register15 = Reg.Register3; }
  public static void movereg_3_16 () { Reg.Register16 = Reg.Register3; }
  public static void movereg_3_17 () { Reg.Register17 = Reg.Register3; }
  public static void movereg_3_18 () { Reg.Register18 = Reg.Register3; }
  public static void movereg_3_19 () { Reg.Register19 = Reg.Register3; }
  public static void movereg_3_20 () { Reg.Register20 = Reg.Register3; }
  public static void movereg_3_21 () { Reg.Register21 = Reg.Register3; }
  public static void movereg_3_22 () { Reg.Register22 = Reg.Register3; }
  public static void movereg_3_23 () { Reg.Register23 = Reg.Register3; }
  public static void movereg_3_24 () { Reg.Register24 = Reg.Register3; }
  public static void movereg_3_25 () { Reg.Register25 = Reg.Register3; }
  public static void movereg_3_26 () { Reg.Register26 = Reg.Register3; }
  public static void movereg_3_27 () { Reg.Register27 = Reg.Register3; }
  public static void movereg_3_28 () { Reg.Register28 = Reg.Register3; }
  public static void movereg_3_29 () { Reg.Register29 = Reg.Register3; }
  public static void movereg_3_30 () { Reg.Register30 = Reg.Register3; }
  public static void movereg_3_31 () { Reg.Register31 = Reg.Register3; }
  public static void movereg_4_1 () { Reg.Register1 = Reg.Register4; }
  public static void movereg_4_2 () { Reg.Register2 = Reg.Register4; }
  public static void movereg_4_3 () { Reg.Register3 = Reg.Register4; }
  public static void movereg_4_5 () { Reg.Register5 = Reg.Register4; }
  public static void movereg_4_6 () { Reg.Register6 = Reg.Register4; }
  public static void movereg_4_7 () { Reg.Register7 = Reg.Register4; }
  public static void movereg_4_8 () { Reg.Register8 = Reg.Register4; }
  public static void movereg_4_9 () { Reg.Register9 = Reg.Register4; }
  public static void movereg_4_10 () { Reg.Register10 = Reg.Register4; }
  public static void movereg_4_11 () { Reg.Register11 = Reg.Register4; }
  public static void movereg_4_12 () { Reg.Register12 = Reg.Register4; }
  public static void movereg_4_13 () { Reg.Register13 = Reg.Register4; }
  public static void movereg_4_14 () { Reg.Register14 = Reg.Register4; }
  public static void movereg_4_15 () { Reg.Register15 = Reg.Register4; }
  public static void movereg_4_16 () { Reg.Register16 = Reg.Register4; }
  public static void movereg_4_17 () { Reg.Register17 = Reg.Register4; }
  public static void movereg_4_18 () { Reg.Register18 = Reg.Register4; }
  public static void movereg_4_19 () { Reg.Register19 = Reg.Register4; }
  public static void movereg_4_20 () { Reg.Register20 = Reg.Register4; }
  public static void movereg_4_21 () { Reg.Register21 = Reg.Register4; }
  public static void movereg_4_22 () { Reg.Register22 = Reg.Register4; }
  public static void movereg_4_23 () { Reg.Register23 = Reg.Register4; }
  public static void movereg_4_24 () { Reg.Register24 = Reg.Register4; }
  public static void movereg_4_25 () { Reg.Register25 = Reg.Register4; }
  public static void movereg_4_26 () { Reg.Register26 = Reg.Register4; }
  public static void movereg_4_27 () { Reg.Register27 = Reg.Register4; }
  public static void movereg_4_28 () { Reg.Register28 = Reg.Register4; }
  public static void movereg_4_29 () { Reg.Register29 = Reg.Register4; }
  public static void movereg_4_30 () { Reg.Register30 = Reg.Register4; }
  public static void movereg_4_31 () { Reg.Register31 = Reg.Register4; }
  public static void movereg_5_1 () { Reg.Register1 = Reg.Register5; }
  public static void movereg_5_2 () { Reg.Register2 = Reg.Register5; }
  public static void movereg_5_3 () { Reg.Register3 = Reg.Register5; }
  public static void movereg_5_4 () { Reg.Register4 = Reg.Register5; }
  public static void movereg_5_6 () { Reg.Register6 = Reg.Register5; }
  public static void movereg_5_7 () { Reg.Register7 = Reg.Register5; }
  public static void movereg_5_8 () { Reg.Register8 = Reg.Register5; }
  public static void movereg_5_9 () { Reg.Register9 = Reg.Register5; }
  public static void movereg_5_10 () { Reg.Register10 = Reg.Register5; }
  public static void movereg_5_11 () { Reg.Register11 = Reg.Register5; }
  public static void movereg_5_12 () { Reg.Register12 = Reg.Register5; }
  public static void movereg_5_13 () { Reg.Register13 = Reg.Register5; }
  public static void movereg_5_14 () { Reg.Register14 = Reg.Register5; }
  public static void movereg_5_15 () { Reg.Register15 = Reg.Register5; }
  public static void movereg_5_16 () { Reg.Register16 = Reg.Register5; }
  public static void movereg_5_17 () { Reg.Register17 = Reg.Register5; }
  public static void movereg_5_18 () { Reg.Register18 = Reg.Register5; }
  public static void movereg_5_19 () { Reg.Register19 = Reg.Register5; }
  public static void movereg_5_20 () { Reg.Register20 = Reg.Register5; }
  public static void movereg_5_21 () { Reg.Register21 = Reg.Register5; }
  public static void movereg_5_22 () { Reg.Register22 = Reg.Register5; }
  public static void movereg_5_23 () { Reg.Register23 = Reg.Register5; }
  public static void movereg_5_24 () { Reg.Register24 = Reg.Register5; }
  public static void movereg_5_25 () { Reg.Register25 = Reg.Register5; }
  public static void movereg_5_26 () { Reg.Register26 = Reg.Register5; }
  public static void movereg_5_27 () { Reg.Register27 = Reg.Register5; }
  public static void movereg_5_28 () { Reg.Register28 = Reg.Register5; }
  public static void movereg_5_29 () { Reg.Register29 = Reg.Register5; }
  public static void movereg_5_30 () { Reg.Register30 = Reg.Register5; }
  public static void movereg_5_31 () { Reg.Register31 = Reg.Register5; }
  public static void movereg_6_1 () { Reg.Register1 = Reg.Register6; }
  public static void movereg_6_2 () { Reg.Register2 = Reg.Register6; }
  public static void movereg_6_3 () { Reg.Register3 = Reg.Register6; }
  public static void movereg_6_4 () { Reg.Register4 = Reg.Register6; }
  public static void movereg_6_5 () { Reg.Register5 = Reg.Register6; }
  public static void movereg_6_7 () { Reg.Register7 = Reg.Register6; }
  public static void movereg_6_8 () { Reg.Register8 = Reg.Register6; }
  public static void movereg_6_9 () { Reg.Register9 = Reg.Register6; }
  public static void movereg_6_10 () { Reg.Register10 = Reg.Register6; }
  public static void movereg_6_11 () { Reg.Register11 = Reg.Register6; }
  public static void movereg_6_12 () { Reg.Register12 = Reg.Register6; }
  public static void movereg_6_13 () { Reg.Register13 = Reg.Register6; }
  public static void movereg_6_14 () { Reg.Register14 = Reg.Register6; }
  public static void movereg_6_15 () { Reg.Register15 = Reg.Register6; }
  public static void movereg_6_16 () { Reg.Register16 = Reg.Register6; }
  public static void movereg_6_17 () { Reg.Register17 = Reg.Register6; }
  public static void movereg_6_18 () { Reg.Register18 = Reg.Register6; }
  public static void movereg_6_19 () { Reg.Register19 = Reg.Register6; }
  public static void movereg_6_20 () { Reg.Register20 = Reg.Register6; }
  public static void movereg_6_21 () { Reg.Register21 = Reg.Register6; }
  public static void movereg_6_22 () { Reg.Register22 = Reg.Register6; }
  public static void movereg_6_23 () { Reg.Register23 = Reg.Register6; }
  public static void movereg_6_24 () { Reg.Register24 = Reg.Register6; }
  public static void movereg_6_25 () { Reg.Register25 = Reg.Register6; }
  public static void movereg_6_26 () { Reg.Register26 = Reg.Register6; }
  public static void movereg_6_27 () { Reg.Register27 = Reg.Register6; }
  public static void movereg_6_28 () { Reg.Register28 = Reg.Register6; }
  public static void movereg_6_29 () { Reg.Register29 = Reg.Register6; }
  public static void movereg_6_30 () { Reg.Register30 = Reg.Register6; }
  public static void movereg_6_31 () { Reg.Register31 = Reg.Register6; }
  public static void movereg_7_1 () { Reg.Register1 = Reg.Register7; }
  public static void movereg_7_2 () { Reg.Register2 = Reg.Register7; }
  public static void movereg_7_3 () { Reg.Register3 = Reg.Register7; }
  public static void movereg_7_4 () { Reg.Register4 = Reg.Register7; }
  public static void movereg_7_5 () { Reg.Register5 = Reg.Register7; }
  public static void movereg_7_6 () { Reg.Register6 = Reg.Register7; }
  public static void movereg_7_8 () { Reg.Register8 = Reg.Register7; }
  public static void movereg_7_9 () { Reg.Register9 = Reg.Register7; }
  public static void movereg_7_10 () { Reg.Register10 = Reg.Register7; }
  public static void movereg_7_11 () { Reg.Register11 = Reg.Register7; }
  public static void movereg_7_12 () { Reg.Register12 = Reg.Register7; }
  public static void movereg_7_13 () { Reg.Register13 = Reg.Register7; }
  public static void movereg_7_14 () { Reg.Register14 = Reg.Register7; }
  public static void movereg_7_15 () { Reg.Register15 = Reg.Register7; }
  public static void movereg_7_16 () { Reg.Register16 = Reg.Register7; }
  public static void movereg_7_17 () { Reg.Register17 = Reg.Register7; }
  public static void movereg_7_18 () { Reg.Register18 = Reg.Register7; }
  public static void movereg_7_19 () { Reg.Register19 = Reg.Register7; }
  public static void movereg_7_20 () { Reg.Register20 = Reg.Register7; }
  public static void movereg_7_21 () { Reg.Register21 = Reg.Register7; }
  public static void movereg_7_22 () { Reg.Register22 = Reg.Register7; }
  public static void movereg_7_23 () { Reg.Register23 = Reg.Register7; }
  public static void movereg_7_24 () { Reg.Register24 = Reg.Register7; }
  public static void movereg_7_25 () { Reg.Register25 = Reg.Register7; }
  public static void movereg_7_26 () { Reg.Register26 = Reg.Register7; }
  public static void movereg_7_27 () { Reg.Register27 = Reg.Register7; }
  public static void movereg_7_28 () { Reg.Register28 = Reg.Register7; }
  public static void movereg_7_29 () { Reg.Register29 = Reg.Register7; }
  public static void movereg_7_30 () { Reg.Register30 = Reg.Register7; }
  public static void movereg_7_31 () { Reg.Register31 = Reg.Register7; }
  public static void movereg_8_1 () { Reg.Register1 = Reg.Register8; }
  public static void movereg_8_2 () { Reg.Register2 = Reg.Register8; }
  public static void movereg_8_3 () { Reg.Register3 = Reg.Register8; }
  public static void movereg_8_4 () { Reg.Register4 = Reg.Register8; }
  public static void movereg_8_5 () { Reg.Register5 = Reg.Register8; }
  public static void movereg_8_6 () { Reg.Register6 = Reg.Register8; }
  public static void movereg_8_7 () { Reg.Register7 = Reg.Register8; }
  public static void movereg_8_9 () { Reg.Register9 = Reg.Register8; }
  public static void movereg_8_10 () { Reg.Register10 = Reg.Register8; }
  public static void movereg_8_11 () { Reg.Register11 = Reg.Register8; }
  public static void movereg_8_12 () { Reg.Register12 = Reg.Register8; }
  public static void movereg_8_13 () { Reg.Register13 = Reg.Register8; }
  public static void movereg_8_14 () { Reg.Register14 = Reg.Register8; }
  public static void movereg_8_15 () { Reg.Register15 = Reg.Register8; }
  public static void movereg_8_16 () { Reg.Register16 = Reg.Register8; }
  public static void movereg_8_17 () { Reg.Register17 = Reg.Register8; }
  public static void movereg_8_18 () { Reg.Register18 = Reg.Register8; }
  public static void movereg_8_19 () { Reg.Register19 = Reg.Register8; }
  public static void movereg_8_20 () { Reg.Register20 = Reg.Register8; }
  public static void movereg_8_21 () { Reg.Register21 = Reg.Register8; }
  public static void movereg_8_22 () { Reg.Register22 = Reg.Register8; }
  public static void movereg_8_23 () { Reg.Register23 = Reg.Register8; }
  public static void movereg_8_24 () { Reg.Register24 = Reg.Register8; }
  public static void movereg_8_25 () { Reg.Register25 = Reg.Register8; }
  public static void movereg_8_26 () { Reg.Register26 = Reg.Register8; }
  public static void movereg_8_27 () { Reg.Register27 = Reg.Register8; }
  public static void movereg_8_28 () { Reg.Register28 = Reg.Register8; }
  public static void movereg_8_29 () { Reg.Register29 = Reg.Register8; }
  public static void movereg_8_30 () { Reg.Register30 = Reg.Register8; }
  public static void movereg_8_31 () { Reg.Register31 = Reg.Register8; }
  public static void movereg_9_1 () { Reg.Register1 = Reg.Register9; }
  public static void movereg_9_2 () { Reg.Register2 = Reg.Register9; }
  public static void movereg_9_3 () { Reg.Register3 = Reg.Register9; }
  public static void movereg_9_4 () { Reg.Register4 = Reg.Register9; }
  public static void movereg_9_5 () { Reg.Register5 = Reg.Register9; }
  public static void movereg_9_6 () { Reg.Register6 = Reg.Register9; }
  public static void movereg_9_7 () { Reg.Register7 = Reg.Register9; }
  public static void movereg_9_8 () { Reg.Register8 = Reg.Register9; }
  public static void movereg_9_10 () { Reg.Register10 = Reg.Register9; }
  public static void movereg_9_11 () { Reg.Register11 = Reg.Register9; }
  public static void movereg_9_12 () { Reg.Register12 = Reg.Register9; }
  public static void movereg_9_13 () { Reg.Register13 = Reg.Register9; }
  public static void movereg_9_14 () { Reg.Register14 = Reg.Register9; }
  public static void movereg_9_15 () { Reg.Register15 = Reg.Register9; }
  public static void movereg_9_16 () { Reg.Register16 = Reg.Register9; }
  public static void movereg_9_17 () { Reg.Register17 = Reg.Register9; }
  public static void movereg_9_18 () { Reg.Register18 = Reg.Register9; }
  public static void movereg_9_19 () { Reg.Register19 = Reg.Register9; }
  public static void movereg_9_20 () { Reg.Register20 = Reg.Register9; }
  public static void movereg_9_21 () { Reg.Register21 = Reg.Register9; }
  public static void movereg_9_22 () { Reg.Register22 = Reg.Register9; }
  public static void movereg_9_23 () { Reg.Register23 = Reg.Register9; }
  public static void movereg_9_24 () { Reg.Register24 = Reg.Register9; }
  public static void movereg_9_25 () { Reg.Register25 = Reg.Register9; }
  public static void movereg_9_26 () { Reg.Register26 = Reg.Register9; }
  public static void movereg_9_27 () { Reg.Register27 = Reg.Register9; }
  public static void movereg_9_28 () { Reg.Register28 = Reg.Register9; }
  public static void movereg_9_29 () { Reg.Register29 = Reg.Register9; }
  public static void movereg_9_30 () { Reg.Register30 = Reg.Register9; }
  public static void movereg_9_31 () { Reg.Register31 = Reg.Register9; }
  public static void movereg_10_1 () { Reg.Register1 = Reg.Register10; }
  public static void movereg_10_2 () { Reg.Register2 = Reg.Register10; }
  public static void movereg_10_3 () { Reg.Register3 = Reg.Register10; }
  public static void movereg_10_4 () { Reg.Register4 = Reg.Register10; }
  public static void movereg_10_5 () { Reg.Register5 = Reg.Register10; }
  public static void movereg_10_6 () { Reg.Register6 = Reg.Register10; }
  public static void movereg_10_7 () { Reg.Register7 = Reg.Register10; }
  public static void movereg_10_8 () { Reg.Register8 = Reg.Register10; }
  public static void movereg_10_9 () { Reg.Register9 = Reg.Register10; }
  public static void movereg_10_11 () { Reg.Register11 = Reg.Register10; }
  public static void movereg_10_12 () { Reg.Register12 = Reg.Register10; }
  public static void movereg_10_13 () { Reg.Register13 = Reg.Register10; }
  public static void movereg_10_14 () { Reg.Register14 = Reg.Register10; }
  public static void movereg_10_15 () { Reg.Register15 = Reg.Register10; }
  public static void movereg_10_16 () { Reg.Register16 = Reg.Register10; }
  public static void movereg_10_17 () { Reg.Register17 = Reg.Register10; }
  public static void movereg_10_18 () { Reg.Register18 = Reg.Register10; }
  public static void movereg_10_19 () { Reg.Register19 = Reg.Register10; }
  public static void movereg_10_20 () { Reg.Register20 = Reg.Register10; }
  public static void movereg_10_21 () { Reg.Register21 = Reg.Register10; }
  public static void movereg_10_22 () { Reg.Register22 = Reg.Register10; }
  public static void movereg_10_23 () { Reg.Register23 = Reg.Register10; }
  public static void movereg_10_24 () { Reg.Register24 = Reg.Register10; }
  public static void movereg_10_25 () { Reg.Register25 = Reg.Register10; }
  public static void movereg_10_26 () { Reg.Register26 = Reg.Register10; }
  public static void movereg_10_27 () { Reg.Register27 = Reg.Register10; }
  public static void movereg_10_28 () { Reg.Register28 = Reg.Register10; }
  public static void movereg_10_29 () { Reg.Register29 = Reg.Register10; }
  public static void movereg_10_30 () { Reg.Register30 = Reg.Register10; }
  public static void movereg_10_31 () { Reg.Register31 = Reg.Register10; }
  public static void movereg_11_1 () { Reg.Register1 = Reg.Register11; }
  public static void movereg_11_2 () { Reg.Register2 = Reg.Register11; }
  public static void movereg_11_3 () { Reg.Register3 = Reg.Register11; }
  public static void movereg_11_4 () { Reg.Register4 = Reg.Register11; }
  public static void movereg_11_5 () { Reg.Register5 = Reg.Register11; }
  public static void movereg_11_6 () { Reg.Register6 = Reg.Register11; }
  public static void movereg_11_7 () { Reg.Register7 = Reg.Register11; }
  public static void movereg_11_8 () { Reg.Register8 = Reg.Register11; }
  public static void movereg_11_9 () { Reg.Register9 = Reg.Register11; }
  public static void movereg_11_10 () { Reg.Register10 = Reg.Register11; }
  public static void movereg_11_12 () { Reg.Register12 = Reg.Register11; }
  public static void movereg_11_13 () { Reg.Register13 = Reg.Register11; }
  public static void movereg_11_14 () { Reg.Register14 = Reg.Register11; }
  public static void movereg_11_15 () { Reg.Register15 = Reg.Register11; }
  public static void movereg_11_16 () { Reg.Register16 = Reg.Register11; }
  public static void movereg_11_17 () { Reg.Register17 = Reg.Register11; }
  public static void movereg_11_18 () { Reg.Register18 = Reg.Register11; }
  public static void movereg_11_19 () { Reg.Register19 = Reg.Register11; }
  public static void movereg_11_20 () { Reg.Register20 = Reg.Register11; }
  public static void movereg_11_21 () { Reg.Register21 = Reg.Register11; }
  public static void movereg_11_22 () { Reg.Register22 = Reg.Register11; }
  public static void movereg_11_23 () { Reg.Register23 = Reg.Register11; }
  public static void movereg_11_24 () { Reg.Register24 = Reg.Register11; }
  public static void movereg_11_25 () { Reg.Register25 = Reg.Register11; }
  public static void movereg_11_26 () { Reg.Register26 = Reg.Register11; }
  public static void movereg_11_27 () { Reg.Register27 = Reg.Register11; }
  public static void movereg_11_28 () { Reg.Register28 = Reg.Register11; }
  public static void movereg_11_29 () { Reg.Register29 = Reg.Register11; }
  public static void movereg_11_30 () { Reg.Register30 = Reg.Register11; }
  public static void movereg_11_31 () { Reg.Register31 = Reg.Register11; }
  public static void movereg_12_1 () { Reg.Register1 = Reg.Register12; }
  public static void movereg_12_2 () { Reg.Register2 = Reg.Register12; }
  public static void movereg_12_3 () { Reg.Register3 = Reg.Register12; }
  public static void movereg_12_4 () { Reg.Register4 = Reg.Register12; }
  public static void movereg_12_5 () { Reg.Register5 = Reg.Register12; }
  public static void movereg_12_6 () { Reg.Register6 = Reg.Register12; }
  public static void movereg_12_7 () { Reg.Register7 = Reg.Register12; }
  public static void movereg_12_8 () { Reg.Register8 = Reg.Register12; }
  public static void movereg_12_9 () { Reg.Register9 = Reg.Register12; }
  public static void movereg_12_10 () { Reg.Register10 = Reg.Register12; }
  public static void movereg_12_11 () { Reg.Register11 = Reg.Register12; }
  public static void movereg_12_13 () { Reg.Register13 = Reg.Register12; }
  public static void movereg_12_14 () { Reg.Register14 = Reg.Register12; }
  public static void movereg_12_15 () { Reg.Register15 = Reg.Register12; }
  public static void movereg_12_16 () { Reg.Register16 = Reg.Register12; }
  public static void movereg_12_17 () { Reg.Register17 = Reg.Register12; }
  public static void movereg_12_18 () { Reg.Register18 = Reg.Register12; }
  public static void movereg_12_19 () { Reg.Register19 = Reg.Register12; }
  public static void movereg_12_20 () { Reg.Register20 = Reg.Register12; }
  public static void movereg_12_21 () { Reg.Register21 = Reg.Register12; }
  public static void movereg_12_22 () { Reg.Register22 = Reg.Register12; }
  public static void movereg_12_23 () { Reg.Register23 = Reg.Register12; }
  public static void movereg_12_24 () { Reg.Register24 = Reg.Register12; }
  public static void movereg_12_25 () { Reg.Register25 = Reg.Register12; }
  public static void movereg_12_26 () { Reg.Register26 = Reg.Register12; }
  public static void movereg_12_27 () { Reg.Register27 = Reg.Register12; }
  public static void movereg_12_28 () { Reg.Register28 = Reg.Register12; }
  public static void movereg_12_29 () { Reg.Register29 = Reg.Register12; }
  public static void movereg_12_30 () { Reg.Register30 = Reg.Register12; }
  public static void movereg_12_31 () { Reg.Register31 = Reg.Register12; }
  public static void movereg_13_1 () { Reg.Register1 = Reg.Register13; }
  public static void movereg_13_2 () { Reg.Register2 = Reg.Register13; }
  public static void movereg_13_3 () { Reg.Register3 = Reg.Register13; }
  public static void movereg_13_4 () { Reg.Register4 = Reg.Register13; }
  public static void movereg_13_5 () { Reg.Register5 = Reg.Register13; }
  public static void movereg_13_6 () { Reg.Register6 = Reg.Register13; }
  public static void movereg_13_7 () { Reg.Register7 = Reg.Register13; }
  public static void movereg_13_8 () { Reg.Register8 = Reg.Register13; }
  public static void movereg_13_9 () { Reg.Register9 = Reg.Register13; }
  public static void movereg_13_10 () { Reg.Register10 = Reg.Register13; }
  public static void movereg_13_11 () { Reg.Register11 = Reg.Register13; }
  public static void movereg_13_12 () { Reg.Register12 = Reg.Register13; }
  public static void movereg_13_14 () { Reg.Register14 = Reg.Register13; }
  public static void movereg_13_15 () { Reg.Register15 = Reg.Register13; }
  public static void movereg_13_16 () { Reg.Register16 = Reg.Register13; }
  public static void movereg_13_17 () { Reg.Register17 = Reg.Register13; }
  public static void movereg_13_18 () { Reg.Register18 = Reg.Register13; }
  public static void movereg_13_19 () { Reg.Register19 = Reg.Register13; }
  public static void movereg_13_20 () { Reg.Register20 = Reg.Register13; }
  public static void movereg_13_21 () { Reg.Register21 = Reg.Register13; }
  public static void movereg_13_22 () { Reg.Register22 = Reg.Register13; }
  public static void movereg_13_23 () { Reg.Register23 = Reg.Register13; }
  public static void movereg_13_24 () { Reg.Register24 = Reg.Register13; }
  public static void movereg_13_25 () { Reg.Register25 = Reg.Register13; }
  public static void movereg_13_26 () { Reg.Register26 = Reg.Register13; }
  public static void movereg_13_27 () { Reg.Register27 = Reg.Register13; }
  public static void movereg_13_28 () { Reg.Register28 = Reg.Register13; }
  public static void movereg_13_29 () { Reg.Register29 = Reg.Register13; }
  public static void movereg_13_30 () { Reg.Register30 = Reg.Register13; }
  public static void movereg_13_31 () { Reg.Register31 = Reg.Register13; }
  public static void movereg_14_1 () { Reg.Register1 = Reg.Register14; }
  public static void movereg_14_2 () { Reg.Register2 = Reg.Register14; }
  public static void movereg_14_3 () { Reg.Register3 = Reg.Register14; }
  public static void movereg_14_4 () { Reg.Register4 = Reg.Register14; }
  public static void movereg_14_5 () { Reg.Register5 = Reg.Register14; }
  public static void movereg_14_6 () { Reg.Register6 = Reg.Register14; }
  public static void movereg_14_7 () { Reg.Register7 = Reg.Register14; }
  public static void movereg_14_8 () { Reg.Register8 = Reg.Register14; }
  public static void movereg_14_9 () { Reg.Register9 = Reg.Register14; }
  public static void movereg_14_10 () { Reg.Register10 = Reg.Register14; }
  public static void movereg_14_11 () { Reg.Register11 = Reg.Register14; }
  public static void movereg_14_12 () { Reg.Register12 = Reg.Register14; }
  public static void movereg_14_13 () { Reg.Register13 = Reg.Register14; }
  public static void movereg_14_15 () { Reg.Register15 = Reg.Register14; }
  public static void movereg_14_16 () { Reg.Register16 = Reg.Register14; }
  public static void movereg_14_17 () { Reg.Register17 = Reg.Register14; }
  public static void movereg_14_18 () { Reg.Register18 = Reg.Register14; }
  public static void movereg_14_19 () { Reg.Register19 = Reg.Register14; }
  public static void movereg_14_20 () { Reg.Register20 = Reg.Register14; }
  public static void movereg_14_21 () { Reg.Register21 = Reg.Register14; }
  public static void movereg_14_22 () { Reg.Register22 = Reg.Register14; }
  public static void movereg_14_23 () { Reg.Register23 = Reg.Register14; }
  public static void movereg_14_24 () { Reg.Register24 = Reg.Register14; }
  public static void movereg_14_25 () { Reg.Register25 = Reg.Register14; }
  public static void movereg_14_26 () { Reg.Register26 = Reg.Register14; }
  public static void movereg_14_27 () { Reg.Register27 = Reg.Register14; }
  public static void movereg_14_28 () { Reg.Register28 = Reg.Register14; }
  public static void movereg_14_29 () { Reg.Register29 = Reg.Register14; }
  public static void movereg_14_30 () { Reg.Register30 = Reg.Register14; }
  public static void movereg_14_31 () { Reg.Register31 = Reg.Register14; }
  public static void movereg_15_1 () { Reg.Register1 = Reg.Register15; }
  public static void movereg_15_2 () { Reg.Register2 = Reg.Register15; }
  public static void movereg_15_3 () { Reg.Register3 = Reg.Register15; }
  public static void movereg_15_4 () { Reg.Register4 = Reg.Register15; }
  public static void movereg_15_5 () { Reg.Register5 = Reg.Register15; }
  public static void movereg_15_6 () { Reg.Register6 = Reg.Register15; }
  public static void movereg_15_7 () { Reg.Register7 = Reg.Register15; }
  public static void movereg_15_8 () { Reg.Register8 = Reg.Register15; }
  public static void movereg_15_9 () { Reg.Register9 = Reg.Register15; }
  public static void movereg_15_10 () { Reg.Register10 = Reg.Register15; }
  public static void movereg_15_11 () { Reg.Register11 = Reg.Register15; }
  public static void movereg_15_12 () { Reg.Register12 = Reg.Register15; }
  public static void movereg_15_13 () { Reg.Register13 = Reg.Register15; }
  public static void movereg_15_14 () { Reg.Register14 = Reg.Register15; }
  public static void movereg_15_16 () { Reg.Register16 = Reg.Register15; }
  public static void movereg_15_17 () { Reg.Register17 = Reg.Register15; }
  public static void movereg_15_18 () { Reg.Register18 = Reg.Register15; }
  public static void movereg_15_19 () { Reg.Register19 = Reg.Register15; }
  public static void movereg_15_20 () { Reg.Register20 = Reg.Register15; }
  public static void movereg_15_21 () { Reg.Register21 = Reg.Register15; }
  public static void movereg_15_22 () { Reg.Register22 = Reg.Register15; }
  public static void movereg_15_23 () { Reg.Register23 = Reg.Register15; }
  public static void movereg_15_24 () { Reg.Register24 = Reg.Register15; }
  public static void movereg_15_25 () { Reg.Register25 = Reg.Register15; }
  public static void movereg_15_26 () { Reg.Register26 = Reg.Register15; }
  public static void movereg_15_27 () { Reg.Register27 = Reg.Register15; }
  public static void movereg_15_28 () { Reg.Register28 = Reg.Register15; }
  public static void movereg_15_29 () { Reg.Register29 = Reg.Register15; }
  public static void movereg_15_30 () { Reg.Register30 = Reg.Register15; }
  public static void movereg_15_31 () { Reg.Register31 = Reg.Register15; }
  public static void movereg_16_1 () { Reg.Register1 = Reg.Register16; }
  public static void movereg_16_2 () { Reg.Register2 = Reg.Register16; }
  public static void movereg_16_3 () { Reg.Register3 = Reg.Register16; }
  public static void movereg_16_4 () { Reg.Register4 = Reg.Register16; }
  public static void movereg_16_5 () { Reg.Register5 = Reg.Register16; }
  public static void movereg_16_6 () { Reg.Register6 = Reg.Register16; }
  public static void movereg_16_7 () { Reg.Register7 = Reg.Register16; }
  public static void movereg_16_8 () { Reg.Register8 = Reg.Register16; }
  public static void movereg_16_9 () { Reg.Register9 = Reg.Register16; }
  public static void movereg_16_10 () { Reg.Register10 = Reg.Register16; }
  public static void movereg_16_11 () { Reg.Register11 = Reg.Register16; }
  public static void movereg_16_12 () { Reg.Register12 = Reg.Register16; }
  public static void movereg_16_13 () { Reg.Register13 = Reg.Register16; }
  public static void movereg_16_14 () { Reg.Register14 = Reg.Register16; }
  public static void movereg_16_15 () { Reg.Register15 = Reg.Register16; }
  public static void movereg_16_17 () { Reg.Register17 = Reg.Register16; }
  public static void movereg_16_18 () { Reg.Register18 = Reg.Register16; }
  public static void movereg_16_19 () { Reg.Register19 = Reg.Register16; }
  public static void movereg_16_20 () { Reg.Register20 = Reg.Register16; }
  public static void movereg_16_21 () { Reg.Register21 = Reg.Register16; }
  public static void movereg_16_22 () { Reg.Register22 = Reg.Register16; }
  public static void movereg_16_23 () { Reg.Register23 = Reg.Register16; }
  public static void movereg_16_24 () { Reg.Register24 = Reg.Register16; }
  public static void movereg_16_25 () { Reg.Register25 = Reg.Register16; }
  public static void movereg_16_26 () { Reg.Register26 = Reg.Register16; }
  public static void movereg_16_27 () { Reg.Register27 = Reg.Register16; }
  public static void movereg_16_28 () { Reg.Register28 = Reg.Register16; }
  public static void movereg_16_29 () { Reg.Register29 = Reg.Register16; }
  public static void movereg_16_30 () { Reg.Register30 = Reg.Register16; }
  public static void movereg_16_31 () { Reg.Register31 = Reg.Register16; }
  public static void movereg_17_1 () { Reg.Register1 = Reg.Register17; }
  public static void movereg_17_2 () { Reg.Register2 = Reg.Register17; }
  public static void movereg_17_3 () { Reg.Register3 = Reg.Register17; }
  public static void movereg_17_4 () { Reg.Register4 = Reg.Register17; }
  public static void movereg_17_5 () { Reg.Register5 = Reg.Register17; }
  public static void movereg_17_6 () { Reg.Register6 = Reg.Register17; }
  public static void movereg_17_7 () { Reg.Register7 = Reg.Register17; }
  public static void movereg_17_8 () { Reg.Register8 = Reg.Register17; }
  public static void movereg_17_9 () { Reg.Register9 = Reg.Register17; }
  public static void movereg_17_10 () { Reg.Register10 = Reg.Register17; }
  public static void movereg_17_11 () { Reg.Register11 = Reg.Register17; }
  public static void movereg_17_12 () { Reg.Register12 = Reg.Register17; }
  public static void movereg_17_13 () { Reg.Register13 = Reg.Register17; }
  public static void movereg_17_14 () { Reg.Register14 = Reg.Register17; }
  public static void movereg_17_15 () { Reg.Register15 = Reg.Register17; }
  public static void movereg_17_16 () { Reg.Register16 = Reg.Register17; }
  public static void movereg_17_18 () { Reg.Register18 = Reg.Register17; }
  public static void movereg_17_19 () { Reg.Register19 = Reg.Register17; }
  public static void movereg_17_20 () { Reg.Register20 = Reg.Register17; }
  public static void movereg_17_21 () { Reg.Register21 = Reg.Register17; }
  public static void movereg_17_22 () { Reg.Register22 = Reg.Register17; }
  public static void movereg_17_23 () { Reg.Register23 = Reg.Register17; }
  public static void movereg_17_24 () { Reg.Register24 = Reg.Register17; }
  public static void movereg_17_25 () { Reg.Register25 = Reg.Register17; }
  public static void movereg_17_26 () { Reg.Register26 = Reg.Register17; }
  public static void movereg_17_27 () { Reg.Register27 = Reg.Register17; }
  public static void movereg_17_28 () { Reg.Register28 = Reg.Register17; }
  public static void movereg_17_29 () { Reg.Register29 = Reg.Register17; }
  public static void movereg_17_30 () { Reg.Register30 = Reg.Register17; }
  public static void movereg_17_31 () { Reg.Register31 = Reg.Register17; }
  public static void movereg_18_1 () { Reg.Register1 = Reg.Register18; }
  public static void movereg_18_2 () { Reg.Register2 = Reg.Register18; }
  public static void movereg_18_3 () { Reg.Register3 = Reg.Register18; }
  public static void movereg_18_4 () { Reg.Register4 = Reg.Register18; }
  public static void movereg_18_5 () { Reg.Register5 = Reg.Register18; }
  public static void movereg_18_6 () { Reg.Register6 = Reg.Register18; }
  public static void movereg_18_7 () { Reg.Register7 = Reg.Register18; }
  public static void movereg_18_8 () { Reg.Register8 = Reg.Register18; }
  public static void movereg_18_9 () { Reg.Register9 = Reg.Register18; }
  public static void movereg_18_10 () { Reg.Register10 = Reg.Register18; }
  public static void movereg_18_11 () { Reg.Register11 = Reg.Register18; }
  public static void movereg_18_12 () { Reg.Register12 = Reg.Register18; }
  public static void movereg_18_13 () { Reg.Register13 = Reg.Register18; }
  public static void movereg_18_14 () { Reg.Register14 = Reg.Register18; }
  public static void movereg_18_15 () { Reg.Register15 = Reg.Register18; }
  public static void movereg_18_16 () { Reg.Register16 = Reg.Register18; }
  public static void movereg_18_17 () { Reg.Register17 = Reg.Register18; }
  public static void movereg_18_19 () { Reg.Register19 = Reg.Register18; }
  public static void movereg_18_20 () { Reg.Register20 = Reg.Register18; }
  public static void movereg_18_21 () { Reg.Register21 = Reg.Register18; }
  public static void movereg_18_22 () { Reg.Register22 = Reg.Register18; }
  public static void movereg_18_23 () { Reg.Register23 = Reg.Register18; }
  public static void movereg_18_24 () { Reg.Register24 = Reg.Register18; }
  public static void movereg_18_25 () { Reg.Register25 = Reg.Register18; }
  public static void movereg_18_26 () { Reg.Register26 = Reg.Register18; }
  public static void movereg_18_27 () { Reg.Register27 = Reg.Register18; }
  public static void movereg_18_28 () { Reg.Register28 = Reg.Register18; }
  public static void movereg_18_29 () { Reg.Register29 = Reg.Register18; }
  public static void movereg_18_30 () { Reg.Register30 = Reg.Register18; }
  public static void movereg_18_31 () { Reg.Register31 = Reg.Register18; }
  public static void movereg_19_1 () { Reg.Register1 = Reg.Register19; }
  public static void movereg_19_2 () { Reg.Register2 = Reg.Register19; }
  public static void movereg_19_3 () { Reg.Register3 = Reg.Register19; }
  public static void movereg_19_4 () { Reg.Register4 = Reg.Register19; }
  public static void movereg_19_5 () { Reg.Register5 = Reg.Register19; }
  public static void movereg_19_6 () { Reg.Register6 = Reg.Register19; }
  public static void movereg_19_7 () { Reg.Register7 = Reg.Register19; }
  public static void movereg_19_8 () { Reg.Register8 = Reg.Register19; }
  public static void movereg_19_9 () { Reg.Register9 = Reg.Register19; }
  public static void movereg_19_10 () { Reg.Register10 = Reg.Register19; }
  public static void movereg_19_11 () { Reg.Register11 = Reg.Register19; }
  public static void movereg_19_12 () { Reg.Register12 = Reg.Register19; }
  public static void movereg_19_13 () { Reg.Register13 = Reg.Register19; }
  public static void movereg_19_14 () { Reg.Register14 = Reg.Register19; }
  public static void movereg_19_15 () { Reg.Register15 = Reg.Register19; }
  public static void movereg_19_16 () { Reg.Register16 = Reg.Register19; }
  public static void movereg_19_17 () { Reg.Register17 = Reg.Register19; }
  public static void movereg_19_18 () { Reg.Register18 = Reg.Register19; }
  public static void movereg_19_20 () { Reg.Register20 = Reg.Register19; }
  public static void movereg_19_21 () { Reg.Register21 = Reg.Register19; }
  public static void movereg_19_22 () { Reg.Register22 = Reg.Register19; }
  public static void movereg_19_23 () { Reg.Register23 = Reg.Register19; }
  public static void movereg_19_24 () { Reg.Register24 = Reg.Register19; }
  public static void movereg_19_25 () { Reg.Register25 = Reg.Register19; }
  public static void movereg_19_26 () { Reg.Register26 = Reg.Register19; }
  public static void movereg_19_27 () { Reg.Register27 = Reg.Register19; }
  public static void movereg_19_28 () { Reg.Register28 = Reg.Register19; }
  public static void movereg_19_29 () { Reg.Register29 = Reg.Register19; }
  public static void movereg_19_30 () { Reg.Register30 = Reg.Register19; }
  public static void movereg_19_31 () { Reg.Register31 = Reg.Register19; }
  public static void movereg_20_1 () { Reg.Register1 = Reg.Register20; }
  public static void movereg_20_2 () { Reg.Register2 = Reg.Register20; }
  public static void movereg_20_3 () { Reg.Register3 = Reg.Register20; }
  public static void movereg_20_4 () { Reg.Register4 = Reg.Register20; }
  public static void movereg_20_5 () { Reg.Register5 = Reg.Register20; }
  public static void movereg_20_6 () { Reg.Register6 = Reg.Register20; }
  public static void movereg_20_7 () { Reg.Register7 = Reg.Register20; }
  public static void movereg_20_8 () { Reg.Register8 = Reg.Register20; }
  public static void movereg_20_9 () { Reg.Register9 = Reg.Register20; }
  public static void movereg_20_10 () { Reg.Register10 = Reg.Register20; }
  public static void movereg_20_11 () { Reg.Register11 = Reg.Register20; }
  public static void movereg_20_12 () { Reg.Register12 = Reg.Register20; }
  public static void movereg_20_13 () { Reg.Register13 = Reg.Register20; }
  public static void movereg_20_14 () { Reg.Register14 = Reg.Register20; }
  public static void movereg_20_15 () { Reg.Register15 = Reg.Register20; }
  public static void movereg_20_16 () { Reg.Register16 = Reg.Register20; }
  public static void movereg_20_17 () { Reg.Register17 = Reg.Register20; }
  public static void movereg_20_18 () { Reg.Register18 = Reg.Register20; }
  public static void movereg_20_19 () { Reg.Register19 = Reg.Register20; }
  public static void movereg_20_21 () { Reg.Register21 = Reg.Register20; }
  public static void movereg_20_22 () { Reg.Register22 = Reg.Register20; }
  public static void movereg_20_23 () { Reg.Register23 = Reg.Register20; }
  public static void movereg_20_24 () { Reg.Register24 = Reg.Register20; }
  public static void movereg_20_25 () { Reg.Register25 = Reg.Register20; }
  public static void movereg_20_26 () { Reg.Register26 = Reg.Register20; }
  public static void movereg_20_27 () { Reg.Register27 = Reg.Register20; }
  public static void movereg_20_28 () { Reg.Register28 = Reg.Register20; }
  public static void movereg_20_29 () { Reg.Register29 = Reg.Register20; }
  public static void movereg_20_30 () { Reg.Register30 = Reg.Register20; }
  public static void movereg_20_31 () { Reg.Register31 = Reg.Register20; }
  public static void movereg_21_1 () { Reg.Register1 = Reg.Register21; }
  public static void movereg_21_2 () { Reg.Register2 = Reg.Register21; }
  public static void movereg_21_3 () { Reg.Register3 = Reg.Register21; }
  public static void movereg_21_4 () { Reg.Register4 = Reg.Register21; }
  public static void movereg_21_5 () { Reg.Register5 = Reg.Register21; }
  public static void movereg_21_6 () { Reg.Register6 = Reg.Register21; }
  public static void movereg_21_7 () { Reg.Register7 = Reg.Register21; }
  public static void movereg_21_8 () { Reg.Register8 = Reg.Register21; }
  public static void movereg_21_9 () { Reg.Register9 = Reg.Register21; }
  public static void movereg_21_10 () { Reg.Register10 = Reg.Register21; }
  public static void movereg_21_11 () { Reg.Register11 = Reg.Register21; }
  public static void movereg_21_12 () { Reg.Register12 = Reg.Register21; }
  public static void movereg_21_13 () { Reg.Register13 = Reg.Register21; }
  public static void movereg_21_14 () { Reg.Register14 = Reg.Register21; }
  public static void movereg_21_15 () { Reg.Register15 = Reg.Register21; }
  public static void movereg_21_16 () { Reg.Register16 = Reg.Register21; }
  public static void movereg_21_17 () { Reg.Register17 = Reg.Register21; }
  public static void movereg_21_18 () { Reg.Register18 = Reg.Register21; }
  public static void movereg_21_19 () { Reg.Register19 = Reg.Register21; }
  public static void movereg_21_20 () { Reg.Register20 = Reg.Register21; }
  public static void movereg_21_22 () { Reg.Register22 = Reg.Register21; }
  public static void movereg_21_23 () { Reg.Register23 = Reg.Register21; }
  public static void movereg_21_24 () { Reg.Register24 = Reg.Register21; }
  public static void movereg_21_25 () { Reg.Register25 = Reg.Register21; }
  public static void movereg_21_26 () { Reg.Register26 = Reg.Register21; }
  public static void movereg_21_27 () { Reg.Register27 = Reg.Register21; }
  public static void movereg_21_28 () { Reg.Register28 = Reg.Register21; }
  public static void movereg_21_29 () { Reg.Register29 = Reg.Register21; }
  public static void movereg_21_30 () { Reg.Register30 = Reg.Register21; }
  public static void movereg_21_31 () { Reg.Register31 = Reg.Register21; }
  public static void movereg_22_1 () { Reg.Register1 = Reg.Register22; }
  public static void movereg_22_2 () { Reg.Register2 = Reg.Register22; }
  public static void movereg_22_3 () { Reg.Register3 = Reg.Register22; }
  public static void movereg_22_4 () { Reg.Register4 = Reg.Register22; }
  public static void movereg_22_5 () { Reg.Register5 = Reg.Register22; }
  public static void movereg_22_6 () { Reg.Register6 = Reg.Register22; }
  public static void movereg_22_7 () { Reg.Register7 = Reg.Register22; }
  public static void movereg_22_8 () { Reg.Register8 = Reg.Register22; }
  public static void movereg_22_9 () { Reg.Register9 = Reg.Register22; }
  public static void movereg_22_10 () { Reg.Register10 = Reg.Register22; }
  public static void movereg_22_11 () { Reg.Register11 = Reg.Register22; }
  public static void movereg_22_12 () { Reg.Register12 = Reg.Register22; }
  public static void movereg_22_13 () { Reg.Register13 = Reg.Register22; }
  public static void movereg_22_14 () { Reg.Register14 = Reg.Register22; }
  public static void movereg_22_15 () { Reg.Register15 = Reg.Register22; }
  public static void movereg_22_16 () { Reg.Register16 = Reg.Register22; }
  public static void movereg_22_17 () { Reg.Register17 = Reg.Register22; }
  public static void movereg_22_18 () { Reg.Register18 = Reg.Register22; }
  public static void movereg_22_19 () { Reg.Register19 = Reg.Register22; }
  public static void movereg_22_20 () { Reg.Register20 = Reg.Register22; }
  public static void movereg_22_21 () { Reg.Register21 = Reg.Register22; }
  public static void movereg_22_23 () { Reg.Register23 = Reg.Register22; }
  public static void movereg_22_24 () { Reg.Register24 = Reg.Register22; }
  public static void movereg_22_25 () { Reg.Register25 = Reg.Register22; }
  public static void movereg_22_26 () { Reg.Register26 = Reg.Register22; }
  public static void movereg_22_27 () { Reg.Register27 = Reg.Register22; }
  public static void movereg_22_28 () { Reg.Register28 = Reg.Register22; }
  public static void movereg_22_29 () { Reg.Register29 = Reg.Register22; }
  public static void movereg_22_30 () { Reg.Register30 = Reg.Register22; }
  public static void movereg_22_31 () { Reg.Register31 = Reg.Register22; }
  public static void movereg_23_1 () { Reg.Register1 = Reg.Register23; }
  public static void movereg_23_2 () { Reg.Register2 = Reg.Register23; }
  public static void movereg_23_3 () { Reg.Register3 = Reg.Register23; }
  public static void movereg_23_4 () { Reg.Register4 = Reg.Register23; }
  public static void movereg_23_5 () { Reg.Register5 = Reg.Register23; }
  public static void movereg_23_6 () { Reg.Register6 = Reg.Register23; }
  public static void movereg_23_7 () { Reg.Register7 = Reg.Register23; }
  public static void movereg_23_8 () { Reg.Register8 = Reg.Register23; }
  public static void movereg_23_9 () { Reg.Register9 = Reg.Register23; }
  public static void movereg_23_10 () { Reg.Register10 = Reg.Register23; }
  public static void movereg_23_11 () { Reg.Register11 = Reg.Register23; }
  public static void movereg_23_12 () { Reg.Register12 = Reg.Register23; }
  public static void movereg_23_13 () { Reg.Register13 = Reg.Register23; }
  public static void movereg_23_14 () { Reg.Register14 = Reg.Register23; }
  public static void movereg_23_15 () { Reg.Register15 = Reg.Register23; }
  public static void movereg_23_16 () { Reg.Register16 = Reg.Register23; }
  public static void movereg_23_17 () { Reg.Register17 = Reg.Register23; }
  public static void movereg_23_18 () { Reg.Register18 = Reg.Register23; }
  public static void movereg_23_19 () { Reg.Register19 = Reg.Register23; }
  public static void movereg_23_20 () { Reg.Register20 = Reg.Register23; }
  public static void movereg_23_21 () { Reg.Register21 = Reg.Register23; }
  public static void movereg_23_22 () { Reg.Register22 = Reg.Register23; }
  public static void movereg_23_24 () { Reg.Register24 = Reg.Register23; }
  public static void movereg_23_25 () { Reg.Register25 = Reg.Register23; }
  public static void movereg_23_26 () { Reg.Register26 = Reg.Register23; }
  public static void movereg_23_27 () { Reg.Register27 = Reg.Register23; }
  public static void movereg_23_28 () { Reg.Register28 = Reg.Register23; }
  public static void movereg_23_29 () { Reg.Register29 = Reg.Register23; }
  public static void movereg_23_30 () { Reg.Register30 = Reg.Register23; }
  public static void movereg_23_31 () { Reg.Register31 = Reg.Register23; }
  public static void movereg_24_1 () { Reg.Register1 = Reg.Register24; }
  public static void movereg_24_2 () { Reg.Register2 = Reg.Register24; }
  public static void movereg_24_3 () { Reg.Register3 = Reg.Register24; }
  public static void movereg_24_4 () { Reg.Register4 = Reg.Register24; }
  public static void movereg_24_5 () { Reg.Register5 = Reg.Register24; }
  public static void movereg_24_6 () { Reg.Register6 = Reg.Register24; }
  public static void movereg_24_7 () { Reg.Register7 = Reg.Register24; }
  public static void movereg_24_8 () { Reg.Register8 = Reg.Register24; }
  public static void movereg_24_9 () { Reg.Register9 = Reg.Register24; }
  public static void movereg_24_10 () { Reg.Register10 = Reg.Register24; }
  public static void movereg_24_11 () { Reg.Register11 = Reg.Register24; }
  public static void movereg_24_12 () { Reg.Register12 = Reg.Register24; }
  public static void movereg_24_13 () { Reg.Register13 = Reg.Register24; }
  public static void movereg_24_14 () { Reg.Register14 = Reg.Register24; }
  public static void movereg_24_15 () { Reg.Register15 = Reg.Register24; }
  public static void movereg_24_16 () { Reg.Register16 = Reg.Register24; }
  public static void movereg_24_17 () { Reg.Register17 = Reg.Register24; }
  public static void movereg_24_18 () { Reg.Register18 = Reg.Register24; }
  public static void movereg_24_19 () { Reg.Register19 = Reg.Register24; }
  public static void movereg_24_20 () { Reg.Register20 = Reg.Register24; }
  public static void movereg_24_21 () { Reg.Register21 = Reg.Register24; }
  public static void movereg_24_22 () { Reg.Register22 = Reg.Register24; }
  public static void movereg_24_23 () { Reg.Register23 = Reg.Register24; }
  public static void movereg_24_25 () { Reg.Register25 = Reg.Register24; }
  public static void movereg_24_26 () { Reg.Register26 = Reg.Register24; }
  public static void movereg_24_27 () { Reg.Register27 = Reg.Register24; }
  public static void movereg_24_28 () { Reg.Register28 = Reg.Register24; }
  public static void movereg_24_29 () { Reg.Register29 = Reg.Register24; }
  public static void movereg_24_30 () { Reg.Register30 = Reg.Register24; }
  public static void movereg_24_31 () { Reg.Register31 = Reg.Register24; }
  public static void movereg_25_1 () { Reg.Register1 = Reg.Register25; }
  public static void movereg_25_2 () { Reg.Register2 = Reg.Register25; }
  public static void movereg_25_3 () { Reg.Register3 = Reg.Register25; }
  public static void movereg_25_4 () { Reg.Register4 = Reg.Register25; }
  public static void movereg_25_5 () { Reg.Register5 = Reg.Register25; }
  public static void movereg_25_6 () { Reg.Register6 = Reg.Register25; }
  public static void movereg_25_7 () { Reg.Register7 = Reg.Register25; }
  public static void movereg_25_8 () { Reg.Register8 = Reg.Register25; }
  public static void movereg_25_9 () { Reg.Register9 = Reg.Register25; }
  public static void movereg_25_10 () { Reg.Register10 = Reg.Register25; }
  public static void movereg_25_11 () { Reg.Register11 = Reg.Register25; }
  public static void movereg_25_12 () { Reg.Register12 = Reg.Register25; }
  public static void movereg_25_13 () { Reg.Register13 = Reg.Register25; }
  public static void movereg_25_14 () { Reg.Register14 = Reg.Register25; }
  public static void movereg_25_15 () { Reg.Register15 = Reg.Register25; }
  public static void movereg_25_16 () { Reg.Register16 = Reg.Register25; }
  public static void movereg_25_17 () { Reg.Register17 = Reg.Register25; }
  public static void movereg_25_18 () { Reg.Register18 = Reg.Register25; }
  public static void movereg_25_19 () { Reg.Register19 = Reg.Register25; }
  public static void movereg_25_20 () { Reg.Register20 = Reg.Register25; }
  public static void movereg_25_21 () { Reg.Register21 = Reg.Register25; }
  public static void movereg_25_22 () { Reg.Register22 = Reg.Register25; }
  public static void movereg_25_23 () { Reg.Register23 = Reg.Register25; }
  public static void movereg_25_24 () { Reg.Register24 = Reg.Register25; }
  public static void movereg_25_26 () { Reg.Register26 = Reg.Register25; }
  public static void movereg_25_27 () { Reg.Register27 = Reg.Register25; }
  public static void movereg_25_28 () { Reg.Register28 = Reg.Register25; }
  public static void movereg_25_29 () { Reg.Register29 = Reg.Register25; }
  public static void movereg_25_30 () { Reg.Register30 = Reg.Register25; }
  public static void movereg_25_31 () { Reg.Register31 = Reg.Register25; }
  public static void movereg_26_1 () { Reg.Register1 = Reg.Register26; }
  public static void movereg_26_2 () { Reg.Register2 = Reg.Register26; }
  public static void movereg_26_3 () { Reg.Register3 = Reg.Register26; }
  public static void movereg_26_4 () { Reg.Register4 = Reg.Register26; }
  public static void movereg_26_5 () { Reg.Register5 = Reg.Register26; }
  public static void movereg_26_6 () { Reg.Register6 = Reg.Register26; }
  public static void movereg_26_7 () { Reg.Register7 = Reg.Register26; }
  public static void movereg_26_8 () { Reg.Register8 = Reg.Register26; }
  public static void movereg_26_9 () { Reg.Register9 = Reg.Register26; }
  public static void movereg_26_10 () { Reg.Register10 = Reg.Register26; }
  public static void movereg_26_11 () { Reg.Register11 = Reg.Register26; }
  public static void movereg_26_12 () { Reg.Register12 = Reg.Register26; }
  public static void movereg_26_13 () { Reg.Register13 = Reg.Register26; }
  public static void movereg_26_14 () { Reg.Register14 = Reg.Register26; }
  public static void movereg_26_15 () { Reg.Register15 = Reg.Register26; }
  public static void movereg_26_16 () { Reg.Register16 = Reg.Register26; }
  public static void movereg_26_17 () { Reg.Register17 = Reg.Register26; }
  public static void movereg_26_18 () { Reg.Register18 = Reg.Register26; }
  public static void movereg_26_19 () { Reg.Register19 = Reg.Register26; }
  public static void movereg_26_20 () { Reg.Register20 = Reg.Register26; }
  public static void movereg_26_21 () { Reg.Register21 = Reg.Register26; }
  public static void movereg_26_22 () { Reg.Register22 = Reg.Register26; }
  public static void movereg_26_23 () { Reg.Register23 = Reg.Register26; }
  public static void movereg_26_24 () { Reg.Register24 = Reg.Register26; }
  public static void movereg_26_25 () { Reg.Register25 = Reg.Register26; }
  public static void movereg_26_27 () { Reg.Register27 = Reg.Register26; }
  public static void movereg_26_28 () { Reg.Register28 = Reg.Register26; }
  public static void movereg_26_29 () { Reg.Register29 = Reg.Register26; }
  public static void movereg_26_30 () { Reg.Register30 = Reg.Register26; }
  public static void movereg_26_31 () { Reg.Register31 = Reg.Register26; }
  public static void movereg_27_1 () { Reg.Register1 = Reg.Register27; }
  public static void movereg_27_2 () { Reg.Register2 = Reg.Register27; }
  public static void movereg_27_3 () { Reg.Register3 = Reg.Register27; }
  public static void movereg_27_4 () { Reg.Register4 = Reg.Register27; }
  public static void movereg_27_5 () { Reg.Register5 = Reg.Register27; }
  public static void movereg_27_6 () { Reg.Register6 = Reg.Register27; }
  public static void movereg_27_7 () { Reg.Register7 = Reg.Register27; }
  public static void movereg_27_8 () { Reg.Register8 = Reg.Register27; }
  public static void movereg_27_9 () { Reg.Register9 = Reg.Register27; }
  public static void movereg_27_10 () { Reg.Register10 = Reg.Register27; }
  public static void movereg_27_11 () { Reg.Register11 = Reg.Register27; }
  public static void movereg_27_12 () { Reg.Register12 = Reg.Register27; }
  public static void movereg_27_13 () { Reg.Register13 = Reg.Register27; }
  public static void movereg_27_14 () { Reg.Register14 = Reg.Register27; }
  public static void movereg_27_15 () { Reg.Register15 = Reg.Register27; }
  public static void movereg_27_16 () { Reg.Register16 = Reg.Register27; }
  public static void movereg_27_17 () { Reg.Register17 = Reg.Register27; }
  public static void movereg_27_18 () { Reg.Register18 = Reg.Register27; }
  public static void movereg_27_19 () { Reg.Register19 = Reg.Register27; }
  public static void movereg_27_20 () { Reg.Register20 = Reg.Register27; }
  public static void movereg_27_21 () { Reg.Register21 = Reg.Register27; }
  public static void movereg_27_22 () { Reg.Register22 = Reg.Register27; }
  public static void movereg_27_23 () { Reg.Register23 = Reg.Register27; }
  public static void movereg_27_24 () { Reg.Register24 = Reg.Register27; }
  public static void movereg_27_25 () { Reg.Register25 = Reg.Register27; }
  public static void movereg_27_26 () { Reg.Register26 = Reg.Register27; }
  public static void movereg_27_28 () { Reg.Register28 = Reg.Register27; }
  public static void movereg_27_29 () { Reg.Register29 = Reg.Register27; }
  public static void movereg_27_30 () { Reg.Register30 = Reg.Register27; }
  public static void movereg_27_31 () { Reg.Register31 = Reg.Register27; }
  public static void movereg_28_1 () { Reg.Register1 = Reg.Register28; }
  public static void movereg_28_2 () { Reg.Register2 = Reg.Register28; }
  public static void movereg_28_3 () { Reg.Register3 = Reg.Register28; }
  public static void movereg_28_4 () { Reg.Register4 = Reg.Register28; }
  public static void movereg_28_5 () { Reg.Register5 = Reg.Register28; }
  public static void movereg_28_6 () { Reg.Register6 = Reg.Register28; }
  public static void movereg_28_7 () { Reg.Register7 = Reg.Register28; }
  public static void movereg_28_8 () { Reg.Register8 = Reg.Register28; }
  public static void movereg_28_9 () { Reg.Register9 = Reg.Register28; }
  public static void movereg_28_10 () { Reg.Register10 = Reg.Register28; }
  public static void movereg_28_11 () { Reg.Register11 = Reg.Register28; }
  public static void movereg_28_12 () { Reg.Register12 = Reg.Register28; }
  public static void movereg_28_13 () { Reg.Register13 = Reg.Register28; }
  public static void movereg_28_14 () { Reg.Register14 = Reg.Register28; }
  public static void movereg_28_15 () { Reg.Register15 = Reg.Register28; }
  public static void movereg_28_16 () { Reg.Register16 = Reg.Register28; }
  public static void movereg_28_17 () { Reg.Register17 = Reg.Register28; }
  public static void movereg_28_18 () { Reg.Register18 = Reg.Register28; }
  public static void movereg_28_19 () { Reg.Register19 = Reg.Register28; }
  public static void movereg_28_20 () { Reg.Register20 = Reg.Register28; }
  public static void movereg_28_21 () { Reg.Register21 = Reg.Register28; }
  public static void movereg_28_22 () { Reg.Register22 = Reg.Register28; }
  public static void movereg_28_23 () { Reg.Register23 = Reg.Register28; }
  public static void movereg_28_24 () { Reg.Register24 = Reg.Register28; }
  public static void movereg_28_25 () { Reg.Register25 = Reg.Register28; }
  public static void movereg_28_26 () { Reg.Register26 = Reg.Register28; }
  public static void movereg_28_27 () { Reg.Register27 = Reg.Register28; }
  public static void movereg_28_29 () { Reg.Register29 = Reg.Register28; }
  public static void movereg_28_30 () { Reg.Register30 = Reg.Register28; }
  public static void movereg_28_31 () { Reg.Register31 = Reg.Register28; }
  public static void movereg_29_1 () { Reg.Register1 = Reg.Register29; }
  public static void movereg_29_2 () { Reg.Register2 = Reg.Register29; }
  public static void movereg_29_3 () { Reg.Register3 = Reg.Register29; }
  public static void movereg_29_4 () { Reg.Register4 = Reg.Register29; }
  public static void movereg_29_5 () { Reg.Register5 = Reg.Register29; }
  public static void movereg_29_6 () { Reg.Register6 = Reg.Register29; }
  public static void movereg_29_7 () { Reg.Register7 = Reg.Register29; }
  public static void movereg_29_8 () { Reg.Register8 = Reg.Register29; }
  public static void movereg_29_9 () { Reg.Register9 = Reg.Register29; }
  public static void movereg_29_10 () { Reg.Register10 = Reg.Register29; }
  public static void movereg_29_11 () { Reg.Register11 = Reg.Register29; }
  public static void movereg_29_12 () { Reg.Register12 = Reg.Register29; }
  public static void movereg_29_13 () { Reg.Register13 = Reg.Register29; }
  public static void movereg_29_14 () { Reg.Register14 = Reg.Register29; }
  public static void movereg_29_15 () { Reg.Register15 = Reg.Register29; }
  public static void movereg_29_16 () { Reg.Register16 = Reg.Register29; }
  public static void movereg_29_17 () { Reg.Register17 = Reg.Register29; }
  public static void movereg_29_18 () { Reg.Register18 = Reg.Register29; }
  public static void movereg_29_19 () { Reg.Register19 = Reg.Register29; }
  public static void movereg_29_20 () { Reg.Register20 = Reg.Register29; }
  public static void movereg_29_21 () { Reg.Register21 = Reg.Register29; }
  public static void movereg_29_22 () { Reg.Register22 = Reg.Register29; }
  public static void movereg_29_23 () { Reg.Register23 = Reg.Register29; }
  public static void movereg_29_24 () { Reg.Register24 = Reg.Register29; }
  public static void movereg_29_25 () { Reg.Register25 = Reg.Register29; }
  public static void movereg_29_26 () { Reg.Register26 = Reg.Register29; }
  public static void movereg_29_27 () { Reg.Register27 = Reg.Register29; }
  public static void movereg_29_28 () { Reg.Register28 = Reg.Register29; }
  public static void movereg_29_30 () { Reg.Register30 = Reg.Register29; }
  public static void movereg_29_31 () { Reg.Register31 = Reg.Register29; }
  public static void movereg_30_1 () { Reg.Register1 = Reg.Register30; }
  public static void movereg_30_2 () { Reg.Register2 = Reg.Register30; }
  public static void movereg_30_3 () { Reg.Register3 = Reg.Register30; }
  public static void movereg_30_4 () { Reg.Register4 = Reg.Register30; }
  public static void movereg_30_5 () { Reg.Register5 = Reg.Register30; }
  public static void movereg_30_6 () { Reg.Register6 = Reg.Register30; }
  public static void movereg_30_7 () { Reg.Register7 = Reg.Register30; }
  public static void movereg_30_8 () { Reg.Register8 = Reg.Register30; }
  public static void movereg_30_9 () { Reg.Register9 = Reg.Register30; }
  public static void movereg_30_10 () { Reg.Register10 = Reg.Register30; }
  public static void movereg_30_11 () { Reg.Register11 = Reg.Register30; }
  public static void movereg_30_12 () { Reg.Register12 = Reg.Register30; }
  public static void movereg_30_13 () { Reg.Register13 = Reg.Register30; }
  public static void movereg_30_14 () { Reg.Register14 = Reg.Register30; }
  public static void movereg_30_15 () { Reg.Register15 = Reg.Register30; }
  public static void movereg_30_16 () { Reg.Register16 = Reg.Register30; }
  public static void movereg_30_17 () { Reg.Register17 = Reg.Register30; }
  public static void movereg_30_18 () { Reg.Register18 = Reg.Register30; }
  public static void movereg_30_19 () { Reg.Register19 = Reg.Register30; }
  public static void movereg_30_20 () { Reg.Register20 = Reg.Register30; }
  public static void movereg_30_21 () { Reg.Register21 = Reg.Register30; }
  public static void movereg_30_22 () { Reg.Register22 = Reg.Register30; }
  public static void movereg_30_23 () { Reg.Register23 = Reg.Register30; }
  public static void movereg_30_24 () { Reg.Register24 = Reg.Register30; }
  public static void movereg_30_25 () { Reg.Register25 = Reg.Register30; }
  public static void movereg_30_26 () { Reg.Register26 = Reg.Register30; }
  public static void movereg_30_27 () { Reg.Register27 = Reg.Register30; }
  public static void movereg_30_28 () { Reg.Register28 = Reg.Register30; }
  public static void movereg_30_29 () { Reg.Register29 = Reg.Register30; }
  public static void movereg_30_31 () { Reg.Register31 = Reg.Register30; }
  public static void movereg_31_1 () { Reg.Register1 = Reg.Register31; }
  public static void movereg_31_2 () { Reg.Register2 = Reg.Register31; }
  public static void movereg_31_3 () { Reg.Register3 = Reg.Register31; }
  public static void movereg_31_4 () { Reg.Register4 = Reg.Register31; }
  public static void movereg_31_5 () { Reg.Register5 = Reg.Register31; }
  public static void movereg_31_6 () { Reg.Register6 = Reg.Register31; }
  public static void movereg_31_7 () { Reg.Register7 = Reg.Register31; }
  public static void movereg_31_8 () { Reg.Register8 = Reg.Register31; }
  public static void movereg_31_9 () { Reg.Register9 = Reg.Register31; }
  public static void movereg_31_10 () { Reg.Register10 = Reg.Register31; }
  public static void movereg_31_11 () { Reg.Register11 = Reg.Register31; }
  public static void movereg_31_12 () { Reg.Register12 = Reg.Register31; }
  public static void movereg_31_13 () { Reg.Register13 = Reg.Register31; }
  public static void movereg_31_14 () { Reg.Register14 = Reg.Register31; }
  public static void movereg_31_15 () { Reg.Register15 = Reg.Register31; }
  public static void movereg_31_16 () { Reg.Register16 = Reg.Register31; }
  public static void movereg_31_17 () { Reg.Register17 = Reg.Register31; }
  public static void movereg_31_18 () { Reg.Register18 = Reg.Register31; }
  public static void movereg_31_19 () { Reg.Register19 = Reg.Register31; }
  public static void movereg_31_20 () { Reg.Register20 = Reg.Register31; }
  public static void movereg_31_21 () { Reg.Register21 = Reg.Register31; }
  public static void movereg_31_22 () { Reg.Register22 = Reg.Register31; }
  public static void movereg_31_23 () { Reg.Register23 = Reg.Register31; }
  public static void movereg_31_24 () { Reg.Register24 = Reg.Register31; }
  public static void movereg_31_25 () { Reg.Register25 = Reg.Register31; }
  public static void movereg_31_26 () { Reg.Register26 = Reg.Register31; }
  public static void movereg_31_27 () { Reg.Register27 = Reg.Register31; }
  public static void movereg_31_28 () { Reg.Register28 = Reg.Register31; }
  public static void movereg_31_29 () { Reg.Register29 = Reg.Register31; }
  public static void movereg_31_30 () { Reg.Register30 = Reg.Register31; }

  /* ===================================================== */
  /*   Call                                                */
  /* ===================================================== */

  /** invoke
   * Pass control to Procedure in Result; fault if
   * Result doesn't contain Procedure; uses trampoline
   */
  public static CodeAddress invoke (int argc)
  {
    Procedure proc = Reg.Result as Procedure;
    if (proc != null)
        return Call.call (proc, argc);
    else
        return Exn.fault (Constants.EX_NONPROC,
                          "invoke: not a procedure",
                          Reg.Result);
  }

  public static CodeAddress invoke0 ()
  {
    Procedure proc = Reg.Result as Procedure;
    if (proc != null)
        return Call.call0 (proc);
    else
        return Exn.fault (Constants.EX_NONPROC,
                          "invoke: not a procedure",
                          Reg.Result);
  }

  public static CodeAddress invoke1 ()
  {
    Procedure proc = Reg.Result as Procedure;
    if (proc != null)
        return Call.call1 (proc);
    else
        return Exn.fault (Constants.EX_NONPROC,
                          "invoke: not a procedure",
                          Reg.Result);
  }

  public static CodeAddress invoke2 ()
  {
    Procedure proc = Reg.Result as Procedure;
    if (proc != null)
        return Call.call2 (proc);
    else
        return Exn.fault (Constants.EX_NONPROC,
                          "invoke: not a procedure",
                          Reg.Result);
  }

  public static CodeAddress invoke3 ()
  {
    Procedure proc = Reg.Result as Procedure;
    if (proc != null)
        return Call.call3 (proc);
    else
        return Exn.fault (Constants.EX_NONPROC,
                          "invoke: not a procedure",
                          Reg.Result);
  }

  public static CodeAddress invoke4 () { return invoke (4); }
  public static CodeAddress invoke5 () { return invoke (5); }
  public static CodeAddress invoke6 () { return invoke (6); }
  public static CodeAddress invoke7 () { return invoke (7); }


  /** apply
   * Pass control to Procedure in Result; fault if Result doesn't
   * contain Procedure; uses trampoline, arranges arguments from
   * list in register k1
   */
  public static CodeAddress apply (int k1, int k2)
  {
    Procedure proc = Reg.Result as Procedure;
    if (proc != null)
        return Call.call (proc, Call.applySetup (k1, k2));
    else
        return Exn.fault (Constants.EX_NONPROC, "apply: not a procedure", Reg.Result);
  }

  /* ===================================================== */
  /*   Closures                                            */
  /* ===================================================== */


  /** lambda
   * Create a new Procedure based on current Procedure (Register 0),
   * given CodeVector, and given number of registers to close over
   */
  public static void lambda (CodeVector codevector, int constIndex, int numRegs)
  {
    Procedure thisProc = Reg.Register0;

    if (numRegs < Reg.LASTREG) {
        Reg.Result = new Procedure (codevector,
				    (SVL) thisProc.constants[constIndex],
				    thisProc,
				    Reg.Close (numRegs));
        }
    else {
        SObject[] environment = new SObject[numRegs+1];
        for (int i = 0; i <= Reg.LASTREG -1; ++i) {
            environment[i] = Reg.getRegister(i);
            }
        SObject restlist = Reg.getRegister (Reg.LASTREG);
        for (int i = Reg.LASTREG; i < numRegs+1; ++i) {
            SPair restpair = (SPair) restlist;
            environment[i] = restpair.first;
            restlist = restpair.rest;
            }
        Reg.Result = new Procedure (codevector,
				    (SVL) thisProc.constants[constIndex],
				    thisProc,
				    environment);
        }
  }

  /** lexes
   * Like lambda, but takes CodeVector and Constants from current Procedure
   */
  public static void lexes(int numRegs)
  {
    Procedure thisProc = Reg.Register0;

    if (numRegs < Reg.LASTREG) {
        Reg.Result = new Procedure (thisProc.entrypoint,
				    (SVL) thisProc.constantvector,
				    thisProc,
				    Reg.Close (numRegs));
        }
    else {
        SObject[] environment = new SObject[numRegs+1];
        for (int i = 0; i <= Reg.LASTREG -1; ++i) {
            environment[i] = Reg.getRegister(i);
            }
        SObject restlist = Reg.getRegister(Reg.LASTREG);
        for (int i = Reg.LASTREG; i < numRegs+1; ++i) {
            SPair restpair = (SPair) restlist;
            environment[i] = restpair.first;
            restlist = restpair.rest;
            }
        Reg.Result = new Procedure(thisProc.entrypoint,
                                   (SVL) thisProc.constantvector,
                                   thisProc,
                                   environment);
        }
  }

  /** lexical
   * Load the value at the specified lexical address into Reg.Result.
   * Inlined by IL version
   */
  public static void lexical (int rib, int slot)
  {
    Reg.Result = Reg.Register0.lookup (rib, slot);
  }

  /** setlex
   * Store Reg.Result in the specified lexical address.
   * Reg.Result is not destroyed in this implementation,
   * contrary to the warning in the MAL docs.
   */
  public static void setlex (int rib, int slot)
  {
    Reg.Register0.update (rib, slot, Reg.Result);
  }

  /** argseq
   * Fault if Reg.Result isn't a number equal to n.
   * Used to check procedure arity.
   */
  public static void argseq (int n)
  {
    if (((SFixnum)(Reg.Result)).value != n)
        Exn.fault(Constants.EX_ARGSEQ, "args= check failed");
  }

  /** argsge
   * FIXME:  Not complete
   * In the docs, r = Reg.NREGS-1 (zero based index of the last register).
   */
  public static void argsge (int n)
  {
    int j = ((SFixnum)(Reg.Result)).value;

    if (n < Reg.NREGS - 2) {
	if (n == j) // a very common case
	    Reg.setRegister (n + 1, Factory.Null);
	else if (j < n) {
	    Exn.faultVarArgCount (n);
	    }
	else if (j < Reg.LASTREG) {
	    // Case 0 in instruction set doc
	    SObject restlist = Factory.Null;
	    for (int i = j; i >= n + 1; i--) {
		restlist = Factory.makePair (Reg.getRegister (i), restlist);
		}
	    Reg.setRegister (n + 1, restlist);
	    }
	else {
            // Case 1 in instruction set doc
            //Exn.msg.WriteLine("argsge case 1: n = {0}; j = {1}", n, j);
            SObject original = Reg.getRegister (Reg.LASTREG);
            SObject copy = Factory.copyList (original);
            SObject restlist = copy;
            for (int i = Reg.LASTREG - 1; i >= n + 1; i--) {
                restlist = Factory.makePair (Reg.getRegister (i), restlist);
                }
            Reg.setRegister (n + 1, restlist);
            }
        }
    else if (j < n) {
        Exn.faultVarArgCount (n);
        }
    else if (n == Reg.NREGS-2) {
        // Case 2 in instruction set doc
        //Exn.msg.WriteLine("argsge case 2: n = {0}; j = {1}", n, j);
        SObject original = Reg.getRegister (Reg.LASTREG);
        SObject copy = Factory.copyList (original);
        Reg.setRegister (Reg.LASTREG, Factory.makePair (copy, Factory.Null));
        }
    else {
        // Case 3 in instruction set doc
        //Exn.msg.WriteLine("argsge case 3: n = {0}; j = {1}", n, j);
        Exn.internalError ("args>= case 3 not implemented");
        }
  }

  /* ===================================================== */
  /*   Continuations                                       */
  /* ===================================================== */

  public static void save (int lastslot)
  {
    Cont.save (lastslot);
  }

  public static void save0 () {Cont.save0 ();}
  public static void save1 () {Cont.save_small (1);}
  public static void save2 () {Cont.save_small (2);}
  public static void save3 () {Cont.save_small (3);}
  public static void save4 () {Cont.save_small (4);}
  public static void save5 () {Cont.save_small (5);}
  public static void save6 () {Cont.save_small (6);}
  public static void save7 () {Cont.save_small (7);}

  public static void pop (int slots)
  {
    Cont.cont.checkPop (slots, Reg.Register0);
    Cont.pop();
  }

  public static void pop0 () { Cont.cont.SafePop (0); }
  public static void pop1 () { Cont.cont.SafePop (1); }
  public static void pop2 () { Cont.cont.SafePop (2); }
  public static void pop3 () { Cont.cont.SafePop (3); }
  public static void pop4 () { Cont.cont.SafePop (4); }
  public static void pop5 () { Cont.cont.SafePop (5); }
  public static void pop6 () { Cont.cont.SafePop (6); }
  public static void pop7 () { Cont.cont.SafePop (7); }

  public static CodeAddress rtn ()
  {
    return Cont.cont.ReturnAddress;
  }

  public static void setrtn (CodeVector code, int index)
  {
    if (code != Cont.cont.Slot0.entrypoint) {
        Exn.internalError("setrtn: different codevector from slot0");
        }
    Cont.cont.returnIndex = index;
  }

  // stack0 is never issued
  public static void stack1 () { Reg.Result = Cont.cont.Slot1; }
  public static void stack2 () { Reg.Result = Cont.cont.Slot2; }
  public static void stack3 () { Reg.Result = Cont.cont.Slot3; }
  public static void stack4 () { Reg.Result = Cont.cont.Slot4; }
  public static void stack5 () { Reg.Result = Cont.cont.Slot5; }
  public static void stack6 () { Reg.Result = Cont.cont.Slot6; }
  public static void stack7 () { Reg.Result = Cont.cont.Slot7; }

  public static void stack (int slot)
  {
    Reg.Result = Cont.cont.getSlot(slot);
  }

  public static void setstk (int slot)
  {
    Cont.cont.setSlot (slot, Reg.Result);
  }

  public static void load (int k, int slot)
  {
    Reg.setRegister (k, Cont.cont.getSlot (slot));
  }

  public static void load_0_0 () { Reg.Register0 = Cont.cont.Slot0; }
  public static void load_1_0 () { Reg.Register1 = Cont.cont.Slot0; }
  public static void load_2_0 () { Reg.Register2 = Cont.cont.Slot0; }
  public static void load_3_0 () { Reg.Register3 = Cont.cont.Slot0; }
  public static void load_4_0 () { Reg.Register4 = Cont.cont.Slot0; }
  public static void load_5_0 () { Reg.Register5 = Cont.cont.Slot0; }
  public static void load_6_0 () { Reg.Register6 = Cont.cont.Slot0; }
  public static void load_7_0 () { Reg.Register7 = Cont.cont.Slot0; }
  public static void load_8_0 () { Reg.Register8 = Cont.cont.Slot0; }
  public static void load_9_0 () { Reg.Register9 = Cont.cont.Slot0; }
  public static void load_10_0 () { Reg.Register10 = Cont.cont.Slot0; }
  public static void load_11_0 () { Reg.Register11 = Cont.cont.Slot0; }
  public static void load_12_0 () { Reg.Register12 = Cont.cont.Slot0; }
  public static void load_13_0 () { Reg.Register13 = Cont.cont.Slot0; }
  public static void load_14_0 () { Reg.Register14 = Cont.cont.Slot0; }
  public static void load_15_0 () { Reg.Register15 = Cont.cont.Slot0; }
  public static void load_16_0 () { Reg.Register16 = Cont.cont.Slot0; }
  public static void load_17_0 () { Reg.Register17 = Cont.cont.Slot0; }
  public static void load_18_0 () { Reg.Register18 = Cont.cont.Slot0; }
  public static void load_19_0 () { Reg.Register19 = Cont.cont.Slot0; }
  public static void load_20_0 () { Reg.Register20 = Cont.cont.Slot0; }
  public static void load_21_0 () { Reg.Register21 = Cont.cont.Slot0; }
  public static void load_22_0 () { Reg.Register22 = Cont.cont.Slot0; }
  public static void load_23_0 () { Reg.Register23 = Cont.cont.Slot0; }
  public static void load_24_0 () { Reg.Register24 = Cont.cont.Slot0; }
  public static void load_25_0 () { Reg.Register25 = Cont.cont.Slot0; }
  public static void load_26_0 () { Reg.Register26 = Cont.cont.Slot0; }
  public static void load_27_0 () { Reg.Register27 = Cont.cont.Slot0; }
  public static void load_28_0 () { Reg.Register28 = Cont.cont.Slot0; }
  public static void load_29_0 () { Reg.Register29 = Cont.cont.Slot0; }
  public static void load_30_0 () { Reg.Register30 = Cont.cont.Slot0; }
  public static void load_31_0 () { Reg.Register31 = Cont.cont.Slot0; }

  public static void load_1_1 () { Reg.Register1 = Cont.cont.Slot1; }
  public static void load_2_1 () { Reg.Register2 = Cont.cont.Slot1; }
  public static void load_3_1 () { Reg.Register3 = Cont.cont.Slot1; }
  public static void load_4_1 () { Reg.Register4 = Cont.cont.Slot1; }
  public static void load_5_1 () { Reg.Register5 = Cont.cont.Slot1; }
  public static void load_6_1 () { Reg.Register6 = Cont.cont.Slot1; }
  public static void load_7_1 () { Reg.Register7 = Cont.cont.Slot1; }
  public static void load_8_1 () { Reg.Register8 = Cont.cont.Slot1; }
  public static void load_9_1 () { Reg.Register9 = Cont.cont.Slot1; }
  public static void load_10_1 () { Reg.Register10 = Cont.cont.Slot1; }
  public static void load_11_1 () { Reg.Register11 = Cont.cont.Slot1; }
  public static void load_12_1 () { Reg.Register12 = Cont.cont.Slot1; }
  public static void load_13_1 () { Reg.Register13 = Cont.cont.Slot1; }
  public static void load_14_1 () { Reg.Register14 = Cont.cont.Slot1; }
  public static void load_15_1 () { Reg.Register15 = Cont.cont.Slot1; }
  public static void load_16_1 () { Reg.Register16 = Cont.cont.Slot1; }
  public static void load_17_1 () { Reg.Register17 = Cont.cont.Slot1; }
  public static void load_18_1 () { Reg.Register18 = Cont.cont.Slot1; }
  public static void load_19_1 () { Reg.Register19 = Cont.cont.Slot1; }
  public static void load_20_1 () { Reg.Register20 = Cont.cont.Slot1; }
  public static void load_21_1 () { Reg.Register21 = Cont.cont.Slot1; }
  public static void load_22_1 () { Reg.Register22 = Cont.cont.Slot1; }
  public static void load_23_1 () { Reg.Register23 = Cont.cont.Slot1; }
  public static void load_24_1 () { Reg.Register24 = Cont.cont.Slot1; }
  public static void load_25_1 () { Reg.Register25 = Cont.cont.Slot1; }
  public static void load_26_1 () { Reg.Register26 = Cont.cont.Slot1; }
  public static void load_27_1 () { Reg.Register27 = Cont.cont.Slot1; }
  public static void load_28_1 () { Reg.Register28 = Cont.cont.Slot1; }
  public static void load_29_1 () { Reg.Register29 = Cont.cont.Slot1; }
  public static void load_30_1 () { Reg.Register30 = Cont.cont.Slot1; }
  public static void load_31_1 () { Reg.Register31 = Cont.cont.Slot1; }
  public static void load_1_2 () { Reg.Register1 = Cont.cont.Slot2; }
  public static void load_2_2 () { Reg.Register2 = Cont.cont.Slot2; }
  public static void load_3_2 () { Reg.Register3 = Cont.cont.Slot2; }
  public static void load_4_2 () { Reg.Register4 = Cont.cont.Slot2; }
  public static void load_5_2 () { Reg.Register5 = Cont.cont.Slot2; }
  public static void load_6_2 () { Reg.Register6 = Cont.cont.Slot2; }
  public static void load_7_2 () { Reg.Register7 = Cont.cont.Slot2; }
  public static void load_8_2 () { Reg.Register8 = Cont.cont.Slot2; }
  public static void load_9_2 () { Reg.Register9 = Cont.cont.Slot2; }
  public static void load_10_2 () { Reg.Register10 = Cont.cont.Slot2; }
  public static void load_11_2 () { Reg.Register11 = Cont.cont.Slot2; }
  public static void load_12_2 () { Reg.Register12 = Cont.cont.Slot2; }
  public static void load_13_2 () { Reg.Register13 = Cont.cont.Slot2; }
  public static void load_14_2 () { Reg.Register14 = Cont.cont.Slot2; }
  public static void load_15_2 () { Reg.Register15 = Cont.cont.Slot2; }
  public static void load_16_2 () { Reg.Register16 = Cont.cont.Slot2; }
  public static void load_17_2 () { Reg.Register17 = Cont.cont.Slot2; }
  public static void load_18_2 () { Reg.Register18 = Cont.cont.Slot2; }
  public static void load_19_2 () { Reg.Register19 = Cont.cont.Slot2; }
  public static void load_20_2 () { Reg.Register20 = Cont.cont.Slot2; }
  public static void load_21_2 () { Reg.Register21 = Cont.cont.Slot2; }
  public static void load_22_2 () { Reg.Register22 = Cont.cont.Slot2; }
  public static void load_23_2 () { Reg.Register23 = Cont.cont.Slot2; }
  public static void load_24_2 () { Reg.Register24 = Cont.cont.Slot2; }
  public static void load_25_2 () { Reg.Register25 = Cont.cont.Slot2; }
  public static void load_26_2 () { Reg.Register26 = Cont.cont.Slot2; }
  public static void load_27_2 () { Reg.Register27 = Cont.cont.Slot2; }
  public static void load_28_2 () { Reg.Register28 = Cont.cont.Slot2; }
  public static void load_29_2 () { Reg.Register29 = Cont.cont.Slot2; }
  public static void load_30_2 () { Reg.Register30 = Cont.cont.Slot2; }
  public static void load_31_2 () { Reg.Register31 = Cont.cont.Slot2; }
  public static void load_1_3 () { Reg.Register1 = Cont.cont.Slot3; }
  public static void load_2_3 () { Reg.Register2 = Cont.cont.Slot3; }
  public static void load_3_3 () { Reg.Register3 = Cont.cont.Slot3; }
  public static void load_4_3 () { Reg.Register4 = Cont.cont.Slot3; }
  public static void load_5_3 () { Reg.Register5 = Cont.cont.Slot3; }
  public static void load_6_3 () { Reg.Register6 = Cont.cont.Slot3; }
  public static void load_7_3 () { Reg.Register7 = Cont.cont.Slot3; }
  public static void load_8_3 () { Reg.Register8 = Cont.cont.Slot3; }
  public static void load_9_3 () { Reg.Register9 = Cont.cont.Slot3; }
  public static void load_10_3 () { Reg.Register10 = Cont.cont.Slot3; }
  public static void load_11_3 () { Reg.Register11 = Cont.cont.Slot3; }
  public static void load_12_3 () { Reg.Register12 = Cont.cont.Slot3; }
  public static void load_13_3 () { Reg.Register13 = Cont.cont.Slot3; }
  public static void load_14_3 () { Reg.Register14 = Cont.cont.Slot3; }
  public static void load_15_3 () { Reg.Register15 = Cont.cont.Slot3; }
  public static void load_16_3 () { Reg.Register16 = Cont.cont.Slot3; }
  public static void load_17_3 () { Reg.Register17 = Cont.cont.Slot3; }
  public static void load_18_3 () { Reg.Register18 = Cont.cont.Slot3; }
  public static void load_19_3 () { Reg.Register19 = Cont.cont.Slot3; }
  public static void load_20_3 () { Reg.Register20 = Cont.cont.Slot3; }
  public static void load_21_3 () { Reg.Register21 = Cont.cont.Slot3; }
  public static void load_22_3 () { Reg.Register22 = Cont.cont.Slot3; }
  public static void load_23_3 () { Reg.Register23 = Cont.cont.Slot3; }
  public static void load_24_3 () { Reg.Register24 = Cont.cont.Slot3; }
  public static void load_25_3 () { Reg.Register25 = Cont.cont.Slot3; }
  public static void load_26_3 () { Reg.Register26 = Cont.cont.Slot3; }
  public static void load_27_3 () { Reg.Register27 = Cont.cont.Slot3; }
  public static void load_28_3 () { Reg.Register28 = Cont.cont.Slot3; }
  public static void load_29_3 () { Reg.Register29 = Cont.cont.Slot3; }
  public static void load_30_3 () { Reg.Register30 = Cont.cont.Slot3; }
  public static void load_31_3 () { Reg.Register31 = Cont.cont.Slot3; }
  public static void load_1_4 () { Reg.Register1 = Cont.cont.Slot4; }
  public static void load_2_4 () { Reg.Register2 = Cont.cont.Slot4; }
  public static void load_3_4 () { Reg.Register3 = Cont.cont.Slot4; }
  public static void load_4_4 () { Reg.Register4 = Cont.cont.Slot4; }
  public static void load_5_4 () { Reg.Register5 = Cont.cont.Slot4; }
  public static void load_6_4 () { Reg.Register6 = Cont.cont.Slot4; }
  public static void load_7_4 () { Reg.Register7 = Cont.cont.Slot4; }
  public static void load_8_4 () { Reg.Register8 = Cont.cont.Slot4; }
  public static void load_9_4 () { Reg.Register9 = Cont.cont.Slot4; }
  public static void load_10_4 () { Reg.Register10 = Cont.cont.Slot4; }
  public static void load_11_4 () { Reg.Register11 = Cont.cont.Slot4; }
  public static void load_12_4 () { Reg.Register12 = Cont.cont.Slot4; }
  public static void load_13_4 () { Reg.Register13 = Cont.cont.Slot4; }
  public static void load_14_4 () { Reg.Register14 = Cont.cont.Slot4; }
  public static void load_15_4 () { Reg.Register15 = Cont.cont.Slot4; }
  public static void load_16_4 () { Reg.Register16 = Cont.cont.Slot4; }
  public static void load_17_4 () { Reg.Register17 = Cont.cont.Slot4; }
  public static void load_18_4 () { Reg.Register18 = Cont.cont.Slot4; }
  public static void load_19_4 () { Reg.Register19 = Cont.cont.Slot4; }
  public static void load_20_4 () { Reg.Register20 = Cont.cont.Slot4; }
  public static void load_21_4 () { Reg.Register21 = Cont.cont.Slot4; }
  public static void load_22_4 () { Reg.Register22 = Cont.cont.Slot4; }
  public static void load_23_4 () { Reg.Register23 = Cont.cont.Slot4; }
  public static void load_24_4 () { Reg.Register24 = Cont.cont.Slot4; }
  public static void load_25_4 () { Reg.Register25 = Cont.cont.Slot4; }
  public static void load_26_4 () { Reg.Register26 = Cont.cont.Slot4; }
  public static void load_27_4 () { Reg.Register27 = Cont.cont.Slot4; }
  public static void load_28_4 () { Reg.Register28 = Cont.cont.Slot4; }
  public static void load_29_4 () { Reg.Register29 = Cont.cont.Slot4; }
  public static void load_30_4 () { Reg.Register30 = Cont.cont.Slot4; }
  public static void load_31_4 () { Reg.Register31 = Cont.cont.Slot4; }
  public static void load_1_5 () { Reg.Register1 = Cont.cont.Slot5; }
  public static void load_2_5 () { Reg.Register2 = Cont.cont.Slot5; }
  public static void load_3_5 () { Reg.Register3 = Cont.cont.Slot5; }
  public static void load_4_5 () { Reg.Register4 = Cont.cont.Slot5; }
  public static void load_5_5 () { Reg.Register5 = Cont.cont.Slot5; }
  public static void load_6_5 () { Reg.Register6 = Cont.cont.Slot5; }
  public static void load_7_5 () { Reg.Register7 = Cont.cont.Slot5; }
  public static void load_8_5 () { Reg.Register8 = Cont.cont.Slot5; }
  public static void load_9_5 () { Reg.Register9 = Cont.cont.Slot5; }
  public static void load_10_5 () { Reg.Register10 = Cont.cont.Slot5; }
  public static void load_11_5 () { Reg.Register11 = Cont.cont.Slot5; }
  public static void load_12_5 () { Reg.Register12 = Cont.cont.Slot5; }
  public static void load_13_5 () { Reg.Register13 = Cont.cont.Slot5; }
  public static void load_14_5 () { Reg.Register14 = Cont.cont.Slot5; }
  public static void load_15_5 () { Reg.Register15 = Cont.cont.Slot5; }
  public static void load_16_5 () { Reg.Register16 = Cont.cont.Slot5; }
  public static void load_17_5 () { Reg.Register17 = Cont.cont.Slot5; }
  public static void load_18_5 () { Reg.Register18 = Cont.cont.Slot5; }
  public static void load_19_5 () { Reg.Register19 = Cont.cont.Slot5; }
  public static void load_20_5 () { Reg.Register20 = Cont.cont.Slot5; }
  public static void load_21_5 () { Reg.Register21 = Cont.cont.Slot5; }
  public static void load_22_5 () { Reg.Register22 = Cont.cont.Slot5; }
  public static void load_23_5 () { Reg.Register23 = Cont.cont.Slot5; }
  public static void load_24_5 () { Reg.Register24 = Cont.cont.Slot5; }
  public static void load_25_5 () { Reg.Register25 = Cont.cont.Slot5; }
  public static void load_26_5 () { Reg.Register26 = Cont.cont.Slot5; }
  public static void load_27_5 () { Reg.Register27 = Cont.cont.Slot5; }
  public static void load_28_5 () { Reg.Register28 = Cont.cont.Slot5; }
  public static void load_29_5 () { Reg.Register29 = Cont.cont.Slot5; }
  public static void load_30_5 () { Reg.Register30 = Cont.cont.Slot5; }
  public static void load_31_5 () { Reg.Register31 = Cont.cont.Slot5; }
  public static void load_1_6 () { Reg.Register1 = Cont.cont.Slot6; }
  public static void load_2_6 () { Reg.Register2 = Cont.cont.Slot6; }
  public static void load_3_6 () { Reg.Register3 = Cont.cont.Slot6; }
  public static void load_4_6 () { Reg.Register4 = Cont.cont.Slot6; }
  public static void load_5_6 () { Reg.Register5 = Cont.cont.Slot6; }
  public static void load_6_6 () { Reg.Register6 = Cont.cont.Slot6; }
  public static void load_7_6 () { Reg.Register7 = Cont.cont.Slot6; }
  public static void load_8_6 () { Reg.Register8 = Cont.cont.Slot6; }
  public static void load_9_6 () { Reg.Register9 = Cont.cont.Slot6; }
  public static void load_10_6 () { Reg.Register10 = Cont.cont.Slot6; }
  public static void load_11_6 () { Reg.Register11 = Cont.cont.Slot6; }
  public static void load_12_6 () { Reg.Register12 = Cont.cont.Slot6; }
  public static void load_13_6 () { Reg.Register13 = Cont.cont.Slot6; }
  public static void load_14_6 () { Reg.Register14 = Cont.cont.Slot6; }
  public static void load_15_6 () { Reg.Register15 = Cont.cont.Slot6; }
  public static void load_16_6 () { Reg.Register16 = Cont.cont.Slot6; }
  public static void load_17_6 () { Reg.Register17 = Cont.cont.Slot6; }
  public static void load_18_6 () { Reg.Register18 = Cont.cont.Slot6; }
  public static void load_19_6 () { Reg.Register19 = Cont.cont.Slot6; }
  public static void load_20_6 () { Reg.Register20 = Cont.cont.Slot6; }
  public static void load_21_6 () { Reg.Register21 = Cont.cont.Slot6; }
  public static void load_22_6 () { Reg.Register22 = Cont.cont.Slot6; }
  public static void load_23_6 () { Reg.Register23 = Cont.cont.Slot6; }
  public static void load_24_6 () { Reg.Register24 = Cont.cont.Slot6; }
  public static void load_25_6 () { Reg.Register25 = Cont.cont.Slot6; }
  public static void load_26_6 () { Reg.Register26 = Cont.cont.Slot6; }
  public static void load_27_6 () { Reg.Register27 = Cont.cont.Slot6; }
  public static void load_28_6 () { Reg.Register28 = Cont.cont.Slot6; }
  public static void load_29_6 () { Reg.Register29 = Cont.cont.Slot6; }
  public static void load_30_6 () { Reg.Register30 = Cont.cont.Slot6; }
  public static void load_31_6 () { Reg.Register31 = Cont.cont.Slot6; }
  public static void load_1_7 () { Reg.Register1 = Cont.cont.Slot7; }
  public static void load_2_7 () { Reg.Register2 = Cont.cont.Slot7; }
  public static void load_3_7 () { Reg.Register3 = Cont.cont.Slot7; }
  public static void load_4_7 () { Reg.Register4 = Cont.cont.Slot7; }
  public static void load_5_7 () { Reg.Register5 = Cont.cont.Slot7; }
  public static void load_6_7 () { Reg.Register6 = Cont.cont.Slot7; }
  public static void load_7_7 () { Reg.Register7 = Cont.cont.Slot7; }
  public static void load_8_7 () { Reg.Register8 = Cont.cont.Slot7; }
  public static void load_9_7 () { Reg.Register9 = Cont.cont.Slot7; }
  public static void load_10_7 () { Reg.Register10 = Cont.cont.Slot7; }
  public static void load_11_7 () { Reg.Register11 = Cont.cont.Slot7; }
  public static void load_12_7 () { Reg.Register12 = Cont.cont.Slot7; }
  public static void load_13_7 () { Reg.Register13 = Cont.cont.Slot7; }
  public static void load_14_7 () { Reg.Register14 = Cont.cont.Slot7; }
  public static void load_15_7 () { Reg.Register15 = Cont.cont.Slot7; }
  public static void load_16_7 () { Reg.Register16 = Cont.cont.Slot7; }
  public static void load_17_7 () { Reg.Register17 = Cont.cont.Slot7; }
  public static void load_18_7 () { Reg.Register18 = Cont.cont.Slot7; }
  public static void load_19_7 () { Reg.Register19 = Cont.cont.Slot7; }
  public static void load_20_7 () { Reg.Register20 = Cont.cont.Slot7; }
  public static void load_21_7 () { Reg.Register21 = Cont.cont.Slot7; }
  public static void load_22_7 () { Reg.Register22 = Cont.cont.Slot7; }
  public static void load_23_7 () { Reg.Register23 = Cont.cont.Slot7; }
  public static void load_24_7 () { Reg.Register24 = Cont.cont.Slot7; }
  public static void load_25_7 () { Reg.Register25 = Cont.cont.Slot7; }
  public static void load_26_7 () { Reg.Register26 = Cont.cont.Slot7; }
  public static void load_27_7 () { Reg.Register27 = Cont.cont.Slot7; }
  public static void load_28_7 () { Reg.Register28 = Cont.cont.Slot7; }
  public static void load_29_7 () { Reg.Register29 = Cont.cont.Slot7; }
  public static void load_30_7 () { Reg.Register30 = Cont.cont.Slot7; }
  public static void load_31_7 () { Reg.Register31 = Cont.cont.Slot7; }

  public static void store (int k, int slot)
  {
    Cont.cont.setSlot (slot, Reg.getRegister (k));
  }

  public static void store_0_0 () { Cont.cont.Slot0 = Reg.Register0; }

  public static void store_1_1 () { Cont.cont.Slot1 = Reg.Register1; }
  public static void store_2_1 () { Cont.cont.Slot1 = Reg.Register2; }
  public static void store_3_1 () { Cont.cont.Slot1 = Reg.Register3; }
  public static void store_4_1 () { Cont.cont.Slot1 = Reg.Register4; }
  public static void store_5_1 () { Cont.cont.Slot1 = Reg.Register5; }
  public static void store_6_1 () { Cont.cont.Slot1 = Reg.Register6; }
  public static void store_7_1 () { Cont.cont.Slot1 = Reg.Register7; }
  public static void store_8_1 () { Cont.cont.Slot1 = Reg.Register8; }
  public static void store_9_1 () { Cont.cont.Slot1 = Reg.Register9; }
  public static void store_10_1 () { Cont.cont.Slot1 = Reg.Register10; }
  public static void store_11_1 () { Cont.cont.Slot1 = Reg.Register11; }
  public static void store_12_1 () { Cont.cont.Slot1 = Reg.Register12; }
  public static void store_13_1 () { Cont.cont.Slot1 = Reg.Register13; }
  public static void store_14_1 () { Cont.cont.Slot1 = Reg.Register14; }
  public static void store_15_1 () { Cont.cont.Slot1 = Reg.Register15; }
  public static void store_16_1 () { Cont.cont.Slot1 = Reg.Register16; }
  public static void store_17_1 () { Cont.cont.Slot1 = Reg.Register17; }
  public static void store_18_1 () { Cont.cont.Slot1 = Reg.Register18; }
  public static void store_19_1 () { Cont.cont.Slot1 = Reg.Register19; }
  public static void store_20_1 () { Cont.cont.Slot1 = Reg.Register20; }
  public static void store_21_1 () { Cont.cont.Slot1 = Reg.Register21; }
  public static void store_22_1 () { Cont.cont.Slot1 = Reg.Register22; }
  public static void store_23_1 () { Cont.cont.Slot1 = Reg.Register23; }
  public static void store_24_1 () { Cont.cont.Slot1 = Reg.Register24; }
  public static void store_25_1 () { Cont.cont.Slot1 = Reg.Register25; }
  public static void store_26_1 () { Cont.cont.Slot1 = Reg.Register26; }
  public static void store_27_1 () { Cont.cont.Slot1 = Reg.Register27; }
  public static void store_28_1 () { Cont.cont.Slot1 = Reg.Register28; }
  public static void store_29_1 () { Cont.cont.Slot1 = Reg.Register29; }
  public static void store_30_1 () { Cont.cont.Slot1 = Reg.Register30; }
  public static void store_31_1 () { Cont.cont.Slot1 = Reg.Register31; }
  public static void store_1_2 () { Cont.cont.Slot2 = Reg.Register1; }
  public static void store_2_2 () { Cont.cont.Slot2 = Reg.Register2; }
  public static void store_3_2 () { Cont.cont.Slot2 = Reg.Register3; }
  public static void store_4_2 () { Cont.cont.Slot2 = Reg.Register4; }
  public static void store_5_2 () { Cont.cont.Slot2 = Reg.Register5; }
  public static void store_6_2 () { Cont.cont.Slot2 = Reg.Register6; }
  public static void store_7_2 () { Cont.cont.Slot2 = Reg.Register7; }
  public static void store_8_2 () { Cont.cont.Slot2 = Reg.Register8; }
  public static void store_9_2 () { Cont.cont.Slot2 = Reg.Register9; }
  public static void store_10_2 () { Cont.cont.Slot2 = Reg.Register10; }
  public static void store_11_2 () { Cont.cont.Slot2 = Reg.Register11; }
  public static void store_12_2 () { Cont.cont.Slot2 = Reg.Register12; }
  public static void store_13_2 () { Cont.cont.Slot2 = Reg.Register13; }
  public static void store_14_2 () { Cont.cont.Slot2 = Reg.Register14; }
  public static void store_15_2 () { Cont.cont.Slot2 = Reg.Register15; }
  public static void store_16_2 () { Cont.cont.Slot2 = Reg.Register16; }
  public static void store_17_2 () { Cont.cont.Slot2 = Reg.Register17; }
  public static void store_18_2 () { Cont.cont.Slot2 = Reg.Register18; }
  public static void store_19_2 () { Cont.cont.Slot2 = Reg.Register19; }
  public static void store_20_2 () { Cont.cont.Slot2 = Reg.Register20; }
  public static void store_21_2 () { Cont.cont.Slot2 = Reg.Register21; }
  public static void store_22_2 () { Cont.cont.Slot2 = Reg.Register22; }
  public static void store_23_2 () { Cont.cont.Slot2 = Reg.Register23; }
  public static void store_24_2 () { Cont.cont.Slot2 = Reg.Register24; }
  public static void store_25_2 () { Cont.cont.Slot2 = Reg.Register25; }
  public static void store_26_2 () { Cont.cont.Slot2 = Reg.Register26; }
  public static void store_27_2 () { Cont.cont.Slot2 = Reg.Register27; }
  public static void store_28_2 () { Cont.cont.Slot2 = Reg.Register28; }
  public static void store_29_2 () { Cont.cont.Slot2 = Reg.Register29; }
  public static void store_30_2 () { Cont.cont.Slot2 = Reg.Register30; }
  public static void store_31_2 () { Cont.cont.Slot2 = Reg.Register31; }
  public static void store_1_3 () { Cont.cont.Slot3 = Reg.Register1; }
  public static void store_2_3 () { Cont.cont.Slot3 = Reg.Register2; }
  public static void store_3_3 () { Cont.cont.Slot3 = Reg.Register3; }
  public static void store_4_3 () { Cont.cont.Slot3 = Reg.Register4; }
  public static void store_5_3 () { Cont.cont.Slot3 = Reg.Register5; }
  public static void store_6_3 () { Cont.cont.Slot3 = Reg.Register6; }
  public static void store_7_3 () { Cont.cont.Slot3 = Reg.Register7; }
  public static void store_8_3 () { Cont.cont.Slot3 = Reg.Register8; }
  public static void store_9_3 () { Cont.cont.Slot3 = Reg.Register9; }
  public static void store_10_3 () { Cont.cont.Slot3 = Reg.Register10; }
  public static void store_11_3 () { Cont.cont.Slot3 = Reg.Register11; }
  public static void store_12_3 () { Cont.cont.Slot3 = Reg.Register12; }
  public static void store_13_3 () { Cont.cont.Slot3 = Reg.Register13; }
  public static void store_14_3 () { Cont.cont.Slot3 = Reg.Register14; }
  public static void store_15_3 () { Cont.cont.Slot3 = Reg.Register15; }
  public static void store_16_3 () { Cont.cont.Slot3 = Reg.Register16; }
  public static void store_17_3 () { Cont.cont.Slot3 = Reg.Register17; }
  public static void store_18_3 () { Cont.cont.Slot3 = Reg.Register18; }
  public static void store_19_3 () { Cont.cont.Slot3 = Reg.Register19; }
  public static void store_20_3 () { Cont.cont.Slot3 = Reg.Register20; }
  public static void store_21_3 () { Cont.cont.Slot3 = Reg.Register21; }
  public static void store_22_3 () { Cont.cont.Slot3 = Reg.Register22; }
  public static void store_23_3 () { Cont.cont.Slot3 = Reg.Register23; }
  public static void store_24_3 () { Cont.cont.Slot3 = Reg.Register24; }
  public static void store_25_3 () { Cont.cont.Slot3 = Reg.Register25; }
  public static void store_26_3 () { Cont.cont.Slot3 = Reg.Register26; }
  public static void store_27_3 () { Cont.cont.Slot3 = Reg.Register27; }
  public static void store_28_3 () { Cont.cont.Slot3 = Reg.Register28; }
  public static void store_29_3 () { Cont.cont.Slot3 = Reg.Register29; }
  public static void store_30_3 () { Cont.cont.Slot3 = Reg.Register30; }
  public static void store_31_3 () { Cont.cont.Slot3 = Reg.Register31; }
  public static void store_1_4 () { Cont.cont.Slot4 = Reg.Register1; }
  public static void store_2_4 () { Cont.cont.Slot4 = Reg.Register2; }
  public static void store_3_4 () { Cont.cont.Slot4 = Reg.Register3; }
  public static void store_4_4 () { Cont.cont.Slot4 = Reg.Register4; }
  public static void store_5_4 () { Cont.cont.Slot4 = Reg.Register5; }
  public static void store_6_4 () { Cont.cont.Slot4 = Reg.Register6; }
  public static void store_7_4 () { Cont.cont.Slot4 = Reg.Register7; }
  public static void store_8_4 () { Cont.cont.Slot4 = Reg.Register8; }
  public static void store_9_4 () { Cont.cont.Slot4 = Reg.Register9; }
  public static void store_10_4 () { Cont.cont.Slot4 = Reg.Register10; }
  public static void store_11_4 () { Cont.cont.Slot4 = Reg.Register11; }
  public static void store_12_4 () { Cont.cont.Slot4 = Reg.Register12; }
  public static void store_13_4 () { Cont.cont.Slot4 = Reg.Register13; }
  public static void store_14_4 () { Cont.cont.Slot4 = Reg.Register14; }
  public static void store_15_4 () { Cont.cont.Slot4 = Reg.Register15; }
  public static void store_16_4 () { Cont.cont.Slot4 = Reg.Register16; }
  public static void store_17_4 () { Cont.cont.Slot4 = Reg.Register17; }
  public static void store_18_4 () { Cont.cont.Slot4 = Reg.Register18; }
  public static void store_19_4 () { Cont.cont.Slot4 = Reg.Register19; }
  public static void store_20_4 () { Cont.cont.Slot4 = Reg.Register20; }
  public static void store_21_4 () { Cont.cont.Slot4 = Reg.Register21; }
  public static void store_22_4 () { Cont.cont.Slot4 = Reg.Register22; }
  public static void store_23_4 () { Cont.cont.Slot4 = Reg.Register23; }
  public static void store_24_4 () { Cont.cont.Slot4 = Reg.Register24; }
  public static void store_25_4 () { Cont.cont.Slot4 = Reg.Register25; }
  public static void store_26_4 () { Cont.cont.Slot4 = Reg.Register26; }
  public static void store_27_4 () { Cont.cont.Slot4 = Reg.Register27; }
  public static void store_28_4 () { Cont.cont.Slot4 = Reg.Register28; }
  public static void store_29_4 () { Cont.cont.Slot4 = Reg.Register29; }
  public static void store_30_4 () { Cont.cont.Slot4 = Reg.Register30; }
  public static void store_31_4 () { Cont.cont.Slot4 = Reg.Register31; }
  public static void store_1_5 () { Cont.cont.Slot5 = Reg.Register1; }
  public static void store_2_5 () { Cont.cont.Slot5 = Reg.Register2; }
  public static void store_3_5 () { Cont.cont.Slot5 = Reg.Register3; }
  public static void store_4_5 () { Cont.cont.Slot5 = Reg.Register4; }
  public static void store_5_5 () { Cont.cont.Slot5 = Reg.Register5; }
  public static void store_6_5 () { Cont.cont.Slot5 = Reg.Register6; }
  public static void store_7_5 () { Cont.cont.Slot5 = Reg.Register7; }
  public static void store_8_5 () { Cont.cont.Slot5 = Reg.Register8; }
  public static void store_9_5 () { Cont.cont.Slot5 = Reg.Register9; }
  public static void store_10_5 () { Cont.cont.Slot5 = Reg.Register10; }
  public static void store_11_5 () { Cont.cont.Slot5 = Reg.Register11; }
  public static void store_12_5 () { Cont.cont.Slot5 = Reg.Register12; }
  public static void store_13_5 () { Cont.cont.Slot5 = Reg.Register13; }
  public static void store_14_5 () { Cont.cont.Slot5 = Reg.Register14; }
  public static void store_15_5 () { Cont.cont.Slot5 = Reg.Register15; }
  public static void store_16_5 () { Cont.cont.Slot5 = Reg.Register16; }
  public static void store_17_5 () { Cont.cont.Slot5 = Reg.Register17; }
  public static void store_18_5 () { Cont.cont.Slot5 = Reg.Register18; }
  public static void store_19_5 () { Cont.cont.Slot5 = Reg.Register19; }
  public static void store_20_5 () { Cont.cont.Slot5 = Reg.Register20; }
  public static void store_21_5 () { Cont.cont.Slot5 = Reg.Register21; }
  public static void store_22_5 () { Cont.cont.Slot5 = Reg.Register22; }
  public static void store_23_5 () { Cont.cont.Slot5 = Reg.Register23; }
  public static void store_24_5 () { Cont.cont.Slot5 = Reg.Register24; }
  public static void store_25_5 () { Cont.cont.Slot5 = Reg.Register25; }
  public static void store_26_5 () { Cont.cont.Slot5 = Reg.Register26; }
  public static void store_27_5 () { Cont.cont.Slot5 = Reg.Register27; }
  public static void store_28_5 () { Cont.cont.Slot5 = Reg.Register28; }
  public static void store_29_5 () { Cont.cont.Slot5 = Reg.Register29; }
  public static void store_30_5 () { Cont.cont.Slot5 = Reg.Register30; }
  public static void store_31_5 () { Cont.cont.Slot5 = Reg.Register31; }
  public static void store_1_6 () { Cont.cont.Slot6 = Reg.Register1; }
  public static void store_2_6 () { Cont.cont.Slot6 = Reg.Register2; }
  public static void store_3_6 () { Cont.cont.Slot6 = Reg.Register3; }
  public static void store_4_6 () { Cont.cont.Slot6 = Reg.Register4; }
  public static void store_5_6 () { Cont.cont.Slot6 = Reg.Register5; }
  public static void store_6_6 () { Cont.cont.Slot6 = Reg.Register6; }
  public static void store_7_6 () { Cont.cont.Slot6 = Reg.Register7; }
  public static void store_8_6 () { Cont.cont.Slot6 = Reg.Register8; }
  public static void store_9_6 () { Cont.cont.Slot6 = Reg.Register9; }
  public static void store_10_6 () { Cont.cont.Slot6 = Reg.Register10; }
  public static void store_11_6 () { Cont.cont.Slot6 = Reg.Register11; }
  public static void store_12_6 () { Cont.cont.Slot6 = Reg.Register12; }
  public static void store_13_6 () { Cont.cont.Slot6 = Reg.Register13; }
  public static void store_14_6 () { Cont.cont.Slot6 = Reg.Register14; }
  public static void store_15_6 () { Cont.cont.Slot6 = Reg.Register15; }
  public static void store_16_6 () { Cont.cont.Slot6 = Reg.Register16; }
  public static void store_17_6 () { Cont.cont.Slot6 = Reg.Register17; }
  public static void store_18_6 () { Cont.cont.Slot6 = Reg.Register18; }
  public static void store_19_6 () { Cont.cont.Slot6 = Reg.Register19; }
  public static void store_20_6 () { Cont.cont.Slot6 = Reg.Register20; }
  public static void store_21_6 () { Cont.cont.Slot6 = Reg.Register21; }
  public static void store_22_6 () { Cont.cont.Slot6 = Reg.Register22; }
  public static void store_23_6 () { Cont.cont.Slot6 = Reg.Register23; }
  public static void store_24_6 () { Cont.cont.Slot6 = Reg.Register24; }
  public static void store_25_6 () { Cont.cont.Slot6 = Reg.Register25; }
  public static void store_26_6 () { Cont.cont.Slot6 = Reg.Register26; }
  public static void store_27_6 () { Cont.cont.Slot6 = Reg.Register27; }
  public static void store_28_6 () { Cont.cont.Slot6 = Reg.Register28; }
  public static void store_29_6 () { Cont.cont.Slot6 = Reg.Register29; }
  public static void store_30_6 () { Cont.cont.Slot6 = Reg.Register30; }
  public static void store_31_6 () { Cont.cont.Slot6 = Reg.Register31; }
  public static void store_1_7 () { Cont.cont.Slot7 = Reg.Register1; }
  public static void store_2_7 () { Cont.cont.Slot7 = Reg.Register2; }
  public static void store_3_7 () { Cont.cont.Slot7 = Reg.Register3; }
  public static void store_4_7 () { Cont.cont.Slot7 = Reg.Register4; }
  public static void store_5_7 () { Cont.cont.Slot7 = Reg.Register5; }
  public static void store_6_7 () { Cont.cont.Slot7 = Reg.Register6; }
  public static void store_7_7 () { Cont.cont.Slot7 = Reg.Register7; }
  public static void store_8_7 () { Cont.cont.Slot7 = Reg.Register8; }
  public static void store_9_7 () { Cont.cont.Slot7 = Reg.Register9; }
  public static void store_10_7 () { Cont.cont.Slot7 = Reg.Register10; }
  public static void store_11_7 () { Cont.cont.Slot7 = Reg.Register11; }
  public static void store_12_7 () { Cont.cont.Slot7 = Reg.Register12; }
  public static void store_13_7 () { Cont.cont.Slot7 = Reg.Register13; }
  public static void store_14_7 () { Cont.cont.Slot7 = Reg.Register14; }
  public static void store_15_7 () { Cont.cont.Slot7 = Reg.Register15; }
  public static void store_16_7 () { Cont.cont.Slot7 = Reg.Register16; }
  public static void store_17_7 () { Cont.cont.Slot7 = Reg.Register17; }
  public static void store_18_7 () { Cont.cont.Slot7 = Reg.Register18; }
  public static void store_19_7 () { Cont.cont.Slot7 = Reg.Register19; }
  public static void store_20_7 () { Cont.cont.Slot7 = Reg.Register20; }
  public static void store_21_7 () { Cont.cont.Slot7 = Reg.Register21; }
  public static void store_22_7 () { Cont.cont.Slot7 = Reg.Register22; }
  public static void store_23_7 () { Cont.cont.Slot7 = Reg.Register23; }
  public static void store_24_7 () { Cont.cont.Slot7 = Reg.Register24; }
  public static void store_25_7 () { Cont.cont.Slot7 = Reg.Register25; }
  public static void store_26_7 () { Cont.cont.Slot7 = Reg.Register26; }
  public static void store_27_7 () { Cont.cont.Slot7 = Reg.Register27; }
  public static void store_28_7 () { Cont.cont.Slot7 = Reg.Register28; }
  public static void store_29_7 () { Cont.cont.Slot7 = Reg.Register29; }
  public static void store_30_7 () { Cont.cont.Slot7 = Reg.Register30; }
  public static void store_31_7 () { Cont.cont.Slot7 = Reg.Register31; }


  /* ===================================================== */
  /*   Exceptions and Reporting                            */
  /* ===================================================== */

  /** trap
   * Documentation is lacking for this instruction.
   * Petit Larceny behavior is partially duplicated here;
   * for now we just cause general fault with error message.
   */
  public static CodeAddress trap (int x, int y, int z, int excode)
  {
    if (x != 0) Reg.Result = Reg.getRegister(x);
    if (y != 0) Reg.Second = Reg.getRegister(y);
    if (z != 0) Reg.Third  = Reg.getRegister(z);
    return Exn.fault (excode, "Trap occurred: " + excode);
  }
}
}
