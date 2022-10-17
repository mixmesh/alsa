// include file to mix variouse sample types


static void PROCEDURE(uint8_t** srcv, size_t m,
		      uint8_t* dst, size_t n
		      PARAMS_DECL)
{    
    LOCALS_DECL
    int i, j;
    uint8_t* vpp[m];
    uint8_t* ptr;
    const int vsize = 128/sizeof(TYPE);

    for (j = 0; j < m; j++)
	vpp[j] = srcv[j];

    while(n >= vsize) {
	ITYPE vec[vsize];

	ptr = vpp[0];
	// load a "vector"
	for (i = 0; i < vsize; i++) {
	    ITYPE sample = READ(ptr);
	    vec[i] = sample;
	    ptr += SAMPLE_SIZE;
	}
	vpp[0] = ptr;
	// add rest of "vectors" to tmp
	for (j = 1; j < m; j++) {
	    ptr = vpp[j];
	    for (i = 0; i < vsize; i++) {
		ITYPE sample = READ(ptr);
		vec[i] = MIX2(vec[i],sample);
		ptr += SAMPLE_SIZE;
	    }
	    vpp[j] = ptr;
	}

	for (i = 0; i < vsize; i++) {
	    WRITE(dst, vec[i]);
	    dst += SAMPLE_SIZE;
	}

	n -= vsize;
    }

    // do tail samples
    if (n > 0) {
	ITYPE vec[n];

	ptr = vpp[0];
	// load a "vector"
	for (i = 0; i < n; i++) {
	    ITYPE sample = READ(ptr);
	    vec[i] = sample;
	    ptr += SAMPLE_SIZE;
	}
	vpp[0] = ptr;
	// add rest of "vectors" to tmp
	for (j = 1; j < m; j++) {
	    ptr = vpp[j];
	    for (i = 0; i < n; i++) {
		ITYPE sample = READ(ptr);
		vec[i] = MIX2(vec[i],sample);
		ptr += SAMPLE_SIZE;
	    }
	    vpp[j] = ptr;
	}

	for (i = 0; i < n; i++) {
	    WRITE(dst, vec[i]);
	    dst += SAMPLE_SIZE;
	}
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
