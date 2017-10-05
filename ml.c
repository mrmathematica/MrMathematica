#include "escheme.h"
#include "mathlink.h"

#define scheme_make_bool(i) (i ? scheme_true : scheme_false)

#define linkpoint(n) SCHEME_CPTR_VAL(argv[n])

static Scheme_Object *warning(int argc, Scheme_Object **argv)
{
	scheme_warning("%T", argv[0]);
	return scheme_void;
}

static Scheme_Object *init_and_openlink(int argc, Scheme_Object **argv)
{
	MLENV ep;
	MLINK lp;
	int err;
	char **arg;
	int i;
	
	arg = calloc( argc+1, sizeof(char*));
	arg[0] = "mrmathematica";
	for(i=0;i<argc;i++)
	{
		arg[i+1] = SCHEME_BYTE_STR_VAL( argv[i]);
	}
	
#if MACINTOSH_MATHLINK
	MLYieldFunctionObject yielder;
	argc = mlmactty_init( &arg);
#endif
	
	ep =  MLInitialize( (MLParametersPointer)0);
	if( ep == (MLENV)0)
	{
		MLDeinitialize( ep);
		scheme_signal_error("MathLink Initialize Error");
	}

#if MACINTOSH_MATHLINK
	yielder = MLCreateYieldFunction( ep, NewMLYielderProc( MLDefaultYielder), 0);
#endif

	lp = MLOpenArgv(ep, arg, arg+(argc+1), &err);
	if( lp == (MLINK)0)
	{
		MLClose( lp);
		scheme_signal_error("MathLink Open Error");
	}

#if MACINTOSH_MATHLINK
	MLSetYieldFunction( lp, yielder);
#endif
	
	free( arg);
	return scheme_make_cptr( lp, scheme_intern_symbol( "MathLink"));
}

static Scheme_Object *MathPutFunction(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( MLPutFunction( linkpoint(2), SCHEME_SYM_VAL( argv[0]), SCHEME_INT_VAL( argv[1])));
}

static Scheme_Object *MathPutArgCount(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( MLPutArgCount( linkpoint(1), SCHEME_INT_VAL( argv[0])));
}

static Scheme_Object *MathPutString(int argc, Scheme_Object **argv)
{
	unsigned short *s;
	long ulen;

	s = scheme_ucs4_to_utf16( SCHEME_CHAR_STR_VAL( argv[0]), 0, SCHEME_CHAR_STRLEN_VAL( argv[0]), s, -1, &ulen, 0);
	return scheme_make_bool( MLPutUnicodeString( linkpoint(1), s, ulen));
}

static Scheme_Object *MathPutByteString(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( MLPutByteString( linkpoint(1), SCHEME_BYTE_STR_VAL( argv[0]), SCHEME_BYTE_STRLEN_VAL( argv[0])));
}

void MathPutNumberHelp(Scheme_Object *num, MLINK lp)
{
	if (SCHEME_INTP( num))
		MLPutInteger( lp, SCHEME_INT_VAL( num));
	else if (SCHEME_DBLP( num))
		MLPutReal( lp, SCHEME_DBL_VAL( num));
	else if (SCHEME_FLTP( num))
		MLPutReal( lp, SCHEME_FLT_VAL( num));
	else if (SCHEME_BIGNUMP( num)) {
		MLPutNext( lp, MLTKINT);
		MLPutString( lp, scheme_bignum_to_string( num, 10));
	} else if (SCHEME_RATIONALP( num)) {
		MLPutFunction( lp, "Rational", 2L);
		MathPutNumberHelp( scheme_rational_numerator( num), lp);
		MathPutNumberHelp( scheme_rational_denominator( num), lp);
	} else if (SCHEME_COMPLEXP( num)) {
		MLPutFunction( lp, "Complex", 2L);
		MathPutNumberHelp( scheme_complex_real_part( num), lp);
		MathPutNumberHelp( scheme_complex_imaginary_part( num), lp);
	}
}	

static Scheme_Object *MathPutNumber(int argc, Scheme_Object **argv)
{
	MathPutNumberHelp( argv[0], linkpoint(1));
	return scheme_void;
}

static Scheme_Object *MathPutNext(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( MLPutNext( linkpoint(1), SCHEME_INT_VAL( argv[0])));
}

static Scheme_Object *MathNextPacket(int argc, Scheme_Object **argv)
{
	return scheme_make_integer( MLNextPacket( linkpoint(0)));
}

static Scheme_Object *MathEndPacket(int argc, Scheme_Object **argv)
{ 
	return scheme_make_bool( MLEndPacket( linkpoint(0)));
}

static Scheme_Object *MathNewPacket(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( MLNewPacket( linkpoint(0)));
}

static Scheme_Object *MathGetString(int argc, Scheme_Object **argv)
{
	unsigned short *s1;
	mzchar *s2;
	long n, ulen;
	
	MLGetUnicodeString( linkpoint(0), &s1, &n);
	s2 = scheme_utf16_to_ucs4(s1, 0, n, s2, -1, &ulen, 1);
	MLDisownUnicodeString( linkpoint(0), s1, n);
	s2[ulen] = 0;
	return scheme_make_sized_char_string( s2, ulen, 0);
}

static Scheme_Object *MathGetByteString(int argc, Scheme_Object **argv)
{
	unsigned char *s;
	long n;
	Scheme_Object *ret;
	
	MLGetByteString ( linkpoint(0), &s, &n, 0L);
	ret = scheme_make_sized_byte_string(s, n, 1);
	MLDisownByteString( linkpoint(0), s, n);
	return ret;
}

static Scheme_Object *MathGetSymbol(int argc, Scheme_Object **argv)
{
	unsigned short *s1;
	char *s2;
	long n;
	int len;
	
	MLGetUnicodeSymbol( linkpoint(0), &s1, &n);
	s2 = (char *)scheme_malloc_atomic( scheme_utf8_encode(s1, 0, n, NULL, 0, 1));
	len = scheme_utf8_encode(s1, 0, n, s2, 0, 1);
	MLDisownUnicodeSymbol( linkpoint(0), s1, n);
	return scheme_intern_exact_symbol( s2, len);
}

static Scheme_Object *MathGetNext(int argc, Scheme_Object **argv)
{
	return scheme_make_integer( MLGetNext( linkpoint(0)));
}

static Scheme_Object *MathGetArgCount(int argc, Scheme_Object **argv)
{
	long len;
	
	MLGetArgCount( linkpoint(0), &len);
	return scheme_make_integer( len);
}

static Scheme_Object *MathClose(int argc, Scheme_Object **argv)
{
	MLClose( linkpoint(0));
	return scheme_void;
}

static Scheme_Object *MathReady(int argc, Scheme_Object **argv)
{
	MLFlush( linkpoint(0));
	return scheme_make_bool( MLReady( linkpoint(0)));
}

static Scheme_Object *MathPutMessage(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( MLPutMessage( linkpoint(1), SCHEME_INT_VAL( argv[0])));
}

static Scheme_Object *MathError(int argc, Scheme_Object **argv)
{
	return scheme_make_integer( MLError( linkpoint(0)));
}

static Scheme_Object *MathErrorMessage(int argc, Scheme_Object **argv)
{
	return scheme_make_byte_string_without_copying( MLErrorMessage( linkpoint(0)));
}

static Scheme_Object *MathClearError(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( MLClearError( linkpoint(0)));
}

static Scheme_Object *SCHEME_MathLinkP(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( SCHEME_CPTRP( argv[0]) && scheme_eq( SCHEME_CPTR_TYPE( argv[0]), scheme_intern_symbol( "MathLink")));
}

static Scheme_Object *MathPutCharSymbol(int argc, Scheme_Object **argv)
{
	return scheme_make_bool( MLPutSymbol( linkpoint(1), SCHEME_SYM_VAL( argv[0])));
}


Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Env *menv;
  Scheme_Object *proc;

  menv = scheme_primitive_module(scheme_intern_symbol("ml"), env);

  proc = scheme_make_prim_w_arity(warning, "warning", 1, 1);
  scheme_add_global("warning", proc, menv);

  proc = scheme_make_prim_w_arity(init_and_openlink, "init_and_openlink", 0, -1);
  scheme_add_global("init_and_openlink", proc, menv);

  proc = scheme_make_prim_w_arity(MathPutFunction, "MathPutFunction", 3, 3);
  scheme_add_global("MathPutFunction", proc, menv);

  proc = scheme_make_prim_w_arity(MathPutArgCount, "MathPutArgCount", 2, 2);
  scheme_add_global("MathPutArgCount", proc, menv);

  proc = scheme_make_prim_w_arity(MathPutString, "MathPutString", 2, 2);
  scheme_add_global("MathPutString", proc, menv);

  proc = scheme_make_prim_w_arity(MathPutNumber, "MathPutNumber", 2, 2);
  scheme_add_global("MathPutNumber", proc, menv);

  proc = scheme_make_prim_w_arity(MathPutNext, "MathPutNext", 2, 2);
  scheme_add_global("MathPutNext", proc, menv);

  proc = scheme_make_prim_w_arity(MathNextPacket, "MathNextPacket", 1, 1);
  scheme_add_global("MathNextPacket", proc, menv);

  proc = scheme_make_prim_w_arity(MathEndPacket, "MathEndPacket", 1, 1);
  scheme_add_global("MathEndPacket", proc, menv);

  proc = scheme_make_prim_w_arity(MathNewPacket, "MathNewPacket", 1, 1);
  scheme_add_global("MathNewPacket", proc, menv);

  proc = scheme_make_prim_w_arity(MathGetString, "MathGetString", 1, 1);
  scheme_add_global("MathGetString", proc, menv);

  proc = scheme_make_prim_w_arity(MathGetSymbol, "MathGetSymbol", 1, 1);
  scheme_add_global("MathGetSymbol", proc, menv);

  proc = scheme_make_prim_w_arity(MathGetNext, "MathGetNext", 1, 1);
  scheme_add_global("MathGetNext", proc, menv);

  proc = scheme_make_prim_w_arity(MathGetArgCount, "MathGetArgCount", 1, 1);
  scheme_add_global("MathGetArgCount", proc, menv);

  proc = scheme_make_prim_w_arity(MathClose, "MathClose", 1, 1);
  scheme_add_global("MathClose", proc, menv);

  proc = scheme_make_prim_w_arity(MathReady, "MathReady", 1, 1);
  scheme_add_global("MathReady", proc, menv);

  proc = scheme_make_prim_w_arity(MathPutMessage, "MathPutMessage", 2, 2);
  scheme_add_global("MathPutMessage", proc, menv);

  proc = scheme_make_prim_w_arity(MathError, "MathError", 1, 1);
  scheme_add_global("MathError", proc, menv);

  proc = scheme_make_prim_w_arity(MathErrorMessage, "MathErrorMessage", 1, 1);
  scheme_add_global("MathErrorMessage", proc, menv);

  proc = scheme_make_prim_w_arity(MathClearError, "MathClearError", 1, 1);
  scheme_add_global("MathClearError", proc, menv);

  proc = scheme_make_prim_w_arity(SCHEME_MathLinkP, "MathLink?", 1, 1);
  scheme_add_global("MathLink?", proc, menv);

  proc = scheme_make_prim_w_arity(MathGetByteString, "MathGetByteString", 1, 1);
  scheme_add_global("MathGetByteString", proc, menv);

  proc = scheme_make_prim_w_arity(MathPutByteString, "MathPutByteString", 2, 2);
  scheme_add_global("MathPutByteString", proc, menv);

  proc = scheme_make_prim_w_arity(MathPutCharSymbol, "MathPutCharSymbol", 2, 2);
  scheme_add_global("MathPutCharSymbol", proc, menv);

  scheme_finish_primitive_module(menv);
  
  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
  return scheme_intern_symbol("ml");
}
