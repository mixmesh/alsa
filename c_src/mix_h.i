// include file to mix variouse sample types

static void PROCEDURE(TYPE** src, size_t m,
		      TYPE* dst, size_t n
		      PARAMS_DECL)
{    
    LOCALS_DECL
    while(n--) {
	int j;
	TYPE_R sum = 0;
	for (j = 0; j < m; j++) {
	    TYPE* ptr = src[j];
	    TYPE_R sample = READ(ptr);
	    sum += sample;
	    src[j] = ptr+1;
	}
	*dst++ = CLAMP(sum);
    }
}

#undef PROCEDURE
#undef TYPE
#undef TYPE_R
#undef PARAMS_DECL
#undef LOCALS_DECL
#undef READ
#undef CLAMP
