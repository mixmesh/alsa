// include file to mix variouse sample types


static void PROCEDURE(TYPE** src, size_t m,
		      TYPE* dst, size_t n
		      PARAMS_DECL)
{    
    LOCALS_DECL
    int i, j;
    TYPE_R tmp[n];
    TYPE_R* tptr;
    TYPE* ptr;
    
    ptr = src[0];
    tptr = tmp;
    for (i = 0; i < n; i++) {
	TYPE sample = READ(ptr);
	*tptr++ = sample;
	ptr++;
    }
    for (j = 1; j < m; j++) {
	ptr = src[j];
	tptr  = tmp;
	for (i = 0; i < n; i++) {
	    TYPE sample = READ(ptr);
	    *tptr = MIX2(*tptr,sample);
	    ptr++;
	    tptr++;
	}
    }
    tptr = tmp;
    for (i = 0; i < n; i++) {
	*dst++ = CLAMP(*tptr++);
    }
}

#undef PROCEDURE
#undef TYPE
#undef TYPE_R
#undef PARAMS_DECL
#undef LOCALS_DECL
#undef READ
#undef CLAMP
#undef MIX2
