/* Expanding the bytecode to threaded code */
/* sestoft@dina.kvl.dk 1999-01-21 */

#include <stdlib.h>
#include <stdio.h>

#include "fail.h"
#include "instruct.h"
#include "misc.h"
#include "unalignd.h"
#include "interp.h"

/* Accessing arguments in the bytecode array: */

#define s16pc s16(pc)
#define u16pc u16(pc)
#define s32pc s32(pc)
#define u32pc u32(pc)
#define SHORT  (sizeof(short))
#define LONG   (sizeof(int32))

#define Instruct(name) case name

/* Computing the length of the realcode array, and building the 
   address offset translation.  

   In the bytecode, a label is represented by a signed 32-bit offset
   from the current bytecode PC; hence the jump is done by 
	pcb = pcb + *pcb

   In the threaded code we want to replace this by the relevant index
   into the realcode array, thus avoiding the addition when doing the
   jump.  At runtime the jump should be done by:
        pcr = *pcr

   When preparing the threaded code we therefore need to know the
   realcode[] index corresponding to the bytecode index pcb + *pcb.
   One way to compute this is to maintain a mapping from all bytecode
   addresses to the corresponding realcode addresses.

   This can be built incrementally while computing the length of the
   realcode corresponding to the bytecode.

   Perhaps this scheme can be optimized, but let's leave that for
   later.
*/

/* Computes and returns the length of the required realcode array;
   allocates and fills in the realaddress array; the argument
   realaddress is a pointer to a variable pointing to that array.  */

int buildrealmap(bytecode_t byteprog, int code_size, int realaddress[])
{
#ifdef THREADED
  int realsize = 0;
  bytecode_t pc = byteprog;    

  /* Initialize to catch errors */
  int i;
  for (i=0; i<code_size; i++)
    realaddress[i] = -1;

  while (pc-byteprog < code_size) {
    int cur_inst = *pc;
    //    printf("%d:%d\t real = %d\n", pc-byteprog, cur_inst, realsize);
    realaddress[pc++ - byteprog] = realsize++;    

    switch (cur_inst) {
      
    /* No arguments: */
    Instruct(SWAP):  
    Instruct(PUSH): 
    Instruct(PUSHACC0):
    Instruct(ACC0): 
    Instruct(PUSHACC1): 
    Instruct(ACC1): 
    Instruct(PUSHACC2): 
    Instruct(ACC2): 
    Instruct(PUSHACC3):
    Instruct(ACC3): 
    Instruct(PUSHACC4):
    Instruct(ACC4): 
    Instruct(PUSHACC5):
    Instruct(ACC5): 
    Instruct(PUSHACC6):
    Instruct(ACC6): 
    Instruct(PUSHACC7):
    Instruct(ACC7): 
    Instruct(PUSHENV1): 
    Instruct(ENV1): 
    Instruct(PUSHENV2): 
    Instruct(ENV2): 
    Instruct(PUSHENV3): 
    Instruct(ENV3): 
    Instruct(PUSHENV4):
    Instruct(ENV4): 
    Instruct(PUSHENV5):
    Instruct(ENV5): 
    Instruct(PUSHENV6):
    Instruct(ENV6): 
    Instruct(PUSHENV7):
    Instruct(ENV7): 
    Instruct(PUSH_ENV1_APPLY1): 
    Instruct(PUSH_ENV1_APPLY2): 
    Instruct(PUSH_ENV1_APPLY3): 
    Instruct(PUSH_ENV1_APPLY4): 
    Instruct(APPLY1): 
    Instruct(APPLY2): 
    Instruct(APPLY3): 
    Instruct(APPLY4): 
    Instruct(RETURN1):
    Instruct(RETURN2):
    Instruct(RESTART): 
    Instruct(UPDATE): 
    Instruct(CHECK_SIGNALS):
    Instruct(PUSHATOM0):
    Instruct(ATOM0):
    Instruct(ATOM1):
    Instruct(ATOM2):
    Instruct(ATOM3):
    Instruct(ATOM4):
    Instruct(ATOM5):
    Instruct(ATOM6):
    Instruct(ATOM7):
    Instruct(ATOM8):
    Instruct(ATOM9):
    Instruct(GETFIELD0):
    Instruct(GETFIELD1):
    Instruct(GETFIELD2):
    Instruct(GETFIELD3):
    Instruct(GETFIELD0_0):
    Instruct(GETFIELD0_1):
    Instruct(GETFIELD1_0):
    Instruct(GETFIELD1_1):
    Instruct(SETFIELD0):
    Instruct(SETFIELD1):
    Instruct(SETFIELD2):
    Instruct(SETFIELD3):
    Instruct(VECTLENGTH):
    Instruct(GETVECTITEM):
    Instruct(SETVECTITEM):
    Instruct(GETSTRINGCHAR):
    Instruct(SETSTRINGCHAR):
    Instruct(BOOLNOT):
    Instruct(POPTRAP):
    Instruct(RAISE):
    Instruct(PUSHCONST0): 
    Instruct(CONST0): 			  
    Instruct(PUSHCONST1): 
    Instruct(CONST1): 
    Instruct(PUSHCONST2): 
    Instruct(CONST2): 
    Instruct(PUSHCONST3): 
    Instruct(CONST3): 
    Instruct(ADDINT):		
    Instruct(SUBINT):		
    Instruct(MULINT):		
    Instruct(DIVINT):		
    Instruct(MODINT):
    Instruct(ANDINT):
    Instruct(ORINT):
    Instruct(XORINT):
    Instruct(SHIFTLEFTINT):
    Instruct(SHIFTRIGHTINTSIGNED):
    Instruct(SHIFTRIGHTINTUNSIGNED):
    Instruct(TAGOF):
    Instruct(EQ):
    Instruct(NEQ):
    Instruct(LTINT):
    Instruct(GTINT):
    Instruct(LEINT):
    Instruct(GEINT):
    Instruct(EQUNSIGN):
    Instruct(NEQUNSIGN):
    Instruct(LTUNSIGN):
    Instruct(GTUNSIGN):
    Instruct(LEUNSIGN):
    Instruct(GEUNSIGN):
    Instruct(FLOATOFINT):
    Instruct(SMLNEGFLOAT):
    Instruct(SMLADDFLOAT):
    Instruct(SMLSUBFLOAT):
    Instruct(SMLMULFLOAT):
    Instruct(SMLDIVFLOAT):
    Instruct(INTOFFLOAT):
    Instruct(EQFLOAT):
    Instruct(NEQFLOAT):
    Instruct(LTFLOAT):
    Instruct(GTFLOAT):
    Instruct(LEFLOAT):
    Instruct(GEFLOAT):
    Instruct(STRINGLENGTH):
    Instruct(EQSTRING):
    Instruct(NEQSTRING):
    Instruct(LTSTRING):
    Instruct(GTSTRING):
    Instruct(LESTRING):
    Instruct(GESTRING):
    Instruct(MAKEVECTOR):
    Instruct(SMLNEGINT):
    Instruct(SMLSUCCINT):
    Instruct(SMLPREDINT):
    Instruct(SMLADDINT):
    Instruct(SMLSUBINT):
    Instruct(SMLMULINT):
    Instruct(SMLDIVINT):
    Instruct(SMLMODINT):
    Instruct(MAKEREFVECTOR):
    Instruct(SMLQUOTINT):
    Instruct(SMLREMINT):
    Instruct(STOP):
      break;

    /* A one-byte argument: */
    Instruct(APPLY): 
    Instruct(GRAB): 
    Instruct(PUSHATOM):
    Instruct(ATOM):
    Instruct(MAKEBLOCK1): 
    Instruct(MAKEBLOCK2): 
    Instruct(MAKEBLOCK3): 
    Instruct(MAKEBLOCK4): 
    Instruct(CONSTBYTE): 
      pc++; realsize++;
      break;

    /* A four-byte label argument.  The label should be translated to
       an address in the realprog[] array.  This requires an auxiliary
       table.  */
    Instruct(PUSH_RETADDR): 
    Instruct(PUSHTRAP):
    Instruct(BRANCH):
    Instruct(BRANCHIF):
    Instruct(BRANCHIFNOT):
    Instruct(POPBRANCHIFNOT):
    Instruct(BRANCHIFEQ):
    Instruct(BRANCHIFNEQ):
    Instruct(BRANCHIFLT):
    Instruct(BRANCHIFGT):
    Instruct(BRANCHIFLE):
    Instruct(BRANCHIFGE):
      pc += LONG; realsize++;
      break;

    /* Two four-byte label arguments.  The labels should
       be translated to an address in the realprog[] array.  This
       requires an auxiliary table.
    */
    Instruct(BRANCHINTERVAL):
      pc += LONG; realsize++;
      pc += LONG; realsize++;
      break;

    /* A two-byte signed argument. */
    Instruct(CONSTSHORT): 
      pc += SHORT; realsize++;
      break;

    /* A two-byte unsigned argument. */
    Instruct(PUSHACC): 
    Instruct(ACCESS): 
    Instruct(POP):
    Instruct(ASSIGN):
    Instruct(PUSHENVACC): 
    Instruct(ENVACC): 
    Instruct(DUMMY): 
    Instruct(RETURN):
    Instruct(SETGLOBAL):
    Instruct(GETGLOBAL):
    Instruct(APPTERM1):
    Instruct(APPTERM2):
    Instruct(APPTERM3):
    Instruct(APPTERM4):
    Instruct(PUSH_ENV1_APPTERM1):
    Instruct(PUSH_ENV1_APPTERM2):
    Instruct(PUSH_ENV1_APPTERM3):
    Instruct(PUSH_ENV1_APPTERM4):
    Instruct(PUSH_GETGLOBAL):
    Instruct(PUSH_GETGLOBAL_APPLY1): 
    Instruct(PUSH_GETGLOBAL_APPLY2): 
    Instruct(PUSH_GETGLOBAL_APPLY3): 
    Instruct(PUSH_GETGLOBAL_APPLY4): 
    Instruct(GETFIELD):
    Instruct(SETFIELD):
    Instruct(C_CALL1):
    Instruct(C_CALL2):
    Instruct(C_CALL3):
    Instruct(C_CALL4):
    Instruct(C_CALL5):
      pc += SHORT; realsize++;
      break;

    /* A four-byte unsigned argument. */
    Instruct(MAKEBLOCK):
      pc += LONG; realsize++;
      break;

    /* A four-byte signed argument. */
    Instruct(PUSHCONSTINT):
    Instruct(CONSTINT):
      pc += LONG; realsize++;
      break;

    /* A one-byte argument and a four-byte signed (label) argument. */
    Instruct(CLOSURE): 
    Instruct(CLOSREC): 
    Instruct(BRANCHIFNEQTAG):
      pc++; realsize++;
      pc += LONG; realsize++;
      break;

    /* A one-byte argument and a two-byte unsigned argument. */
    Instruct(APPTERM): 
    Instruct(C_CALLN):
      pc++; realsize++;
      pc += SHORT; realsize++;
      break;

    /* Two two-byte unsigned arguments. */
    Instruct(PUSH_GETGLOBAL_APPTERM1):
    Instruct(PUSH_GETGLOBAL_APPTERM2):
    Instruct(PUSH_GETGLOBAL_APPTERM3):
    Instruct(PUSH_GETGLOBAL_APPTERM4):
      pc += SHORT; realsize++;
      pc += SHORT; realsize++;
      break;

    /* A one-byte argument and a table of four-byte signed (label) arguments */
    /* We keep the byte argument for consistency.                           */
    Instruct(SWITCH):
      {
	int n = *pc;
	pc++; realsize++;		/* The byte     */
	pc += n * LONG; realsize += n;	/* The n labels */
      }
      break;
    default:
      printf("buildrealmap: opcode = %d at %d\n", *pc, pc-byteprog);
      fatal_error("bad opcode\n");
    }
  }
  //  printf("buildrealmap finished\n");
  return realsize;
#else
  return 0;			/* bogus */
#endif
} // buildrealmap

/* Computing the real address from the bytecode address and an offset */

#define REALADDR(pc, offset) \
  realprog + realaddress[(int)(pc - byteprog) + (int)offset];

/* The expander itself */
/* Returns a pointer to the realcode array (an array of C code addresses). */

realcode_t expandcode(bytecode_t byteprog, int code_size, void * jumptable[])
{
#ifdef THREADED
  bytecode_t pc = byteprog;
  int *realaddress = (int*)(malloc(code_size * sizeof(int)));
  int realsize = buildrealmap(byteprog, code_size, realaddress);
  realcode_t realprog = (realcode_t)malloc(realsize * sizeof(void*));
  int codeptr = 0;

  while (pc - byteprog < code_size) {
    //    printf("%d:%d\t\n", pc-byteprog, *pc);
    switch (*pc) {

    /* No arguments: */
    Instruct(SWAP):  
    Instruct(PUSH): 
    Instruct(PUSHACC0):
    Instruct(ACC0): 
    Instruct(PUSHACC1): 
    Instruct(ACC1): 
    Instruct(PUSHACC2): 
    Instruct(ACC2): 
    Instruct(PUSHACC3):
    Instruct(ACC3): 
    Instruct(PUSHACC4):
    Instruct(ACC4): 
    Instruct(PUSHACC5):
    Instruct(ACC5): 
    Instruct(PUSHACC6):
    Instruct(ACC6): 
    Instruct(PUSHACC7):
    Instruct(ACC7): 
    Instruct(PUSHENV1): 
    Instruct(ENV1): 
    Instruct(PUSHENV2): 
    Instruct(ENV2): 
    Instruct(PUSHENV3): 
    Instruct(ENV3): 
    Instruct(PUSHENV4):
    Instruct(ENV4): 
    Instruct(PUSHENV5):
    Instruct(ENV5): 
    Instruct(PUSHENV6):
    Instruct(ENV6): 
    Instruct(PUSHENV7):
    Instruct(ENV7): 
    Instruct(PUSH_ENV1_APPLY1): 
    Instruct(PUSH_ENV1_APPLY2): 
    Instruct(PUSH_ENV1_APPLY3): 
    Instruct(PUSH_ENV1_APPLY4): 
    Instruct(APPLY1): 
    Instruct(APPLY2): 
    Instruct(APPLY3): 
    Instruct(APPLY4): 
    Instruct(RETURN1):
    Instruct(RETURN2):
    Instruct(RESTART): 
    Instruct(UPDATE): 
    Instruct(CHECK_SIGNALS):
    Instruct(PUSHATOM0):
    Instruct(ATOM0):
    Instruct(ATOM1):
    Instruct(ATOM2):
    Instruct(ATOM3):
    Instruct(ATOM4):
    Instruct(ATOM5):
    Instruct(ATOM6):
    Instruct(ATOM7):
    Instruct(ATOM8):
    Instruct(ATOM9):
    Instruct(GETFIELD0):
    Instruct(GETFIELD1):
    Instruct(GETFIELD2):
    Instruct(GETFIELD3):
    Instruct(GETFIELD0_0):
    Instruct(GETFIELD0_1):
    Instruct(GETFIELD1_0):
    Instruct(GETFIELD1_1):
    Instruct(SETFIELD0):
    Instruct(SETFIELD1):
    Instruct(SETFIELD2):
    Instruct(SETFIELD3):
    Instruct(VECTLENGTH):
    Instruct(GETVECTITEM):
    Instruct(SETVECTITEM):
    Instruct(GETSTRINGCHAR):
    Instruct(SETSTRINGCHAR):
    Instruct(BOOLNOT):
    Instruct(POPTRAP):
    Instruct(RAISE):
    Instruct(PUSHCONST0): 
    Instruct(CONST0): 			  
    Instruct(PUSHCONST1): 
    Instruct(CONST1): 
    Instruct(PUSHCONST2): 
    Instruct(CONST2): 
    Instruct(PUSHCONST3): 
    Instruct(CONST3): 
    Instruct(ADDINT):		
    Instruct(SUBINT):		
    Instruct(MULINT):		
    Instruct(DIVINT):		
    Instruct(MODINT):
    Instruct(ANDINT):
    Instruct(ORINT):
    Instruct(XORINT):
    Instruct(SHIFTLEFTINT):
    Instruct(SHIFTRIGHTINTSIGNED):
    Instruct(SHIFTRIGHTINTUNSIGNED):
    Instruct(TAGOF):
    Instruct(EQ):
    Instruct(NEQ):
    Instruct(LTINT):
    Instruct(GTINT):
    Instruct(LEINT):
    Instruct(GEINT):
    Instruct(EQUNSIGN):
    Instruct(NEQUNSIGN):
    Instruct(LTUNSIGN):
    Instruct(GTUNSIGN):
    Instruct(LEUNSIGN):
    Instruct(GEUNSIGN):
    Instruct(FLOATOFINT):
    Instruct(SMLNEGFLOAT):
    Instruct(SMLADDFLOAT):
    Instruct(SMLSUBFLOAT):
    Instruct(SMLMULFLOAT):
    Instruct(SMLDIVFLOAT):
    Instruct(INTOFFLOAT):
    Instruct(EQFLOAT):
    Instruct(NEQFLOAT):
    Instruct(LTFLOAT):
    Instruct(GTFLOAT):
    Instruct(LEFLOAT):
    Instruct(GEFLOAT):
    Instruct(STRINGLENGTH):
    Instruct(EQSTRING):
    Instruct(NEQSTRING):
    Instruct(LTSTRING):
    Instruct(GTSTRING):
    Instruct(LESTRING):
    Instruct(GESTRING):
    Instruct(MAKEVECTOR):
    Instruct(SMLNEGINT):
    Instruct(SMLSUCCINT):
    Instruct(SMLPREDINT):
    Instruct(SMLADDINT):
    Instruct(SMLSUBINT):
    Instruct(SMLMULINT):
    Instruct(SMLDIVINT):
    Instruct(SMLMODINT):
    Instruct(MAKEREFVECTOR):
    Instruct(SMLQUOTINT):
    Instruct(SMLREMINT):
    Instruct(STOP):
      realprog[codeptr++] = jumptable[*pc++]; 
      break;

    /* A one-byte argument: */
    Instruct(APPLY): 
    Instruct(GRAB): 
    Instruct(PUSHATOM):
    Instruct(ATOM):
    Instruct(MAKEBLOCK1): 
    Instruct(MAKEBLOCK2): 
    Instruct(MAKEBLOCK3): 
    Instruct(MAKEBLOCK4): 
    Instruct(CONSTBYTE): 
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = (void*)(long)(*pc++); 
      break;

    /* A four-byte label argument.  The label is translated to an index
       into the realprog[] array. */
    Instruct(PUSH_RETADDR): 
    Instruct(PUSHTRAP):
    Instruct(BRANCH):
    Instruct(BRANCHIF):
    Instruct(BRANCHIFNOT):
    Instruct(POPBRANCHIFNOT):
    Instruct(BRANCHIFEQ):
    Instruct(BRANCHIFNEQ):
    Instruct(BRANCHIFLT):
    Instruct(BRANCHIFGT):
    Instruct(BRANCHIFLE):
    Instruct(BRANCHIFGE):
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = REALADDR(pc, s32pc); pc += LONG;
      break;

    /* Two four-byte label arguments.  The labels should
       be translated to an address in the realprog[] array.  This
       requires an auxiliary table.
    */
    Instruct(BRANCHINTERVAL):
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = REALADDR(pc, s32pc); 
      pc += LONG;
      realprog[codeptr++] = REALADDR(pc, s32pc); 
      pc += LONG;
      break;

    /* A two-byte signed argument. */
    Instruct(CONSTSHORT): 
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = (void*)(long)s16pc;
      pc += SHORT;
      break;

    /* A two-byte unsigned argument. */
    Instruct(PUSHACC): 
    Instruct(ACCESS): 
    Instruct(POP):
    Instruct(ASSIGN):
    Instruct(PUSHENVACC): 
    Instruct(ENVACC): 
    Instruct(DUMMY): 
    Instruct(RETURN):
    Instruct(SETGLOBAL):
    Instruct(GETGLOBAL):
    Instruct(APPTERM1):
    Instruct(APPTERM2):
    Instruct(APPTERM3):
    Instruct(APPTERM4):
    Instruct(PUSH_ENV1_APPTERM1):
    Instruct(PUSH_ENV1_APPTERM2):
    Instruct(PUSH_ENV1_APPTERM3):
    Instruct(PUSH_ENV1_APPTERM4):
    Instruct(PUSH_GETGLOBAL):
    Instruct(PUSH_GETGLOBAL_APPLY1): 
    Instruct(PUSH_GETGLOBAL_APPLY2): 
    Instruct(PUSH_GETGLOBAL_APPLY3): 
    Instruct(PUSH_GETGLOBAL_APPLY4): 
    Instruct(GETFIELD):
    Instruct(SETFIELD):
    Instruct(C_CALL1):
    Instruct(C_CALL2):
    Instruct(C_CALL3):
    Instruct(C_CALL4):
    Instruct(C_CALL5):
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = (void*)(unsigned long)u16pc;
      pc += SHORT;
      break;

    /* A four-byte unsigned argument. */
    Instruct(MAKEBLOCK):
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = (void*)(unsigned long)u32pc;
      pc += LONG;
      break;

    /* A four-byte signed argument. */
    Instruct(PUSHCONSTINT):
    Instruct(CONSTINT):
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = (void*)(long)s32pc;
      pc += LONG;
      break;

    /* A one-byte argument and a four-byte signed (label) argument. */
    Instruct(CLOSURE): 
    Instruct(CLOSREC): 
    Instruct(BRANCHIFNEQTAG):
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = (void*)(unsigned long)*pc++;
      realprog[codeptr++] = REALADDR(pc, s32pc);
      pc += LONG;
      break;

    /* A one-byte argument and a two-byte unsigned argument. */
    Instruct(APPTERM): 
    Instruct(C_CALLN):
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = (void*)(long)*pc++;
      realprog[codeptr++] = (void*)(unsigned long)u16pc;
      pc += SHORT;
      break;

    /* Two two-byte unsigned arguments. */
    Instruct(PUSH_GETGLOBAL_APPTERM1):
    Instruct(PUSH_GETGLOBAL_APPTERM2):
    Instruct(PUSH_GETGLOBAL_APPTERM3):
    Instruct(PUSH_GETGLOBAL_APPTERM4):
      realprog[codeptr++] = jumptable[*pc++]; 
      realprog[codeptr++] = (void*)(unsigned long)u16pc; pc += SHORT;
      realprog[codeptr++] = (void*)(unsigned long)u16pc; pc += SHORT;
      break;

    /* A one-byte argument and a table of four-byte signed (label) arguments. */
    /* We keep the byte argument for consistency.                            */
    Instruct(SWITCH):
      {
	unsigned long i, n;
	bytecode_t pc1;
	realprog[codeptr++] = jumptable[*pc++]; 
	n = (unsigned long)*pc++;
	realprog[codeptr++] = (void*)n; 
	pc1 = pc;
	for (i=0; i<n; i++) {
	  realprog[codeptr++] = REALADDR(pc1, s32pc);
	  pc += LONG;
	}
      }
      break;
    default:
      printf("expandcode: opcode = %d at %d\n", *pc, pc-byteprog);
      fatal_error("bad opcode");
    }
  }
  //  printf("expandcode finished\n");
  free(realaddress);
  //  printf("freed realaddress\n");
  return realprog;
#else
  return NULL;
#endif
} // expandcode
