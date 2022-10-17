// include file to mix variouse sample types

static void PROCEDURE(uint8_t** src, size_t m,
		      uint8_t* dst, size_t n
		      PARAMS_DECL)
{    
    LOCALS_DECL
    int offs = 0;
    while(n--) {
	int j;
	ITYPE sum = 0;
	for (j = 0; j < m; j++) {
	    TYPE* ptr = (TYPE*)(src[j]+offs);
	    ITYPE sample = READ(ptr);
	    sum = MIX2(sum, sample);
	}
	WRITE(dst, sum);
	dst += SAMPLE_SIZE;
	offs += SAMPLE_SIZE;
    }
}

#undef PROCEDURE
#undef TYPE
#undef ITYPE
#undef SAMPLE_SIZE
#undef PARAMS_DECL
#undef LOCALS_DECL
#undef READ
#undef WRITE
#undef CLAMP
#undef MIX2
