INFILE=Side_Right.wav
OPTS=-t wav

sr:
	sox $(INFILE) $(OPTS) -b 8   sr_u8.wav
	sox $(INFILE) $(OPTS) -b 32 -L sr_s32_le.wav
	sox $(INFILE) $(OPTS) -b 32 -B  sr_s32_be.wav
	sox $(INFILE) $(OPTS) -b 16 -L sr_s16_le.wav
	sox $(INFILE) $(OPTS) -b 16 -B  sr_s16_be.wav
	sox $(INFILE) $(OPTS) -L -b 24 sr_s24_3le.wav
	sox $(INFILE) $(OPTS) -L -b 32 -e floating-point sr_float_le.wav
	sox $(INFILE) $(OPTS) -B -b 32 -e floating-point sr_float_be.wav
	sox $(INFILE) $(OPTS) -B -b 64 -e floating-point sr_float64_be.wav
	sox $(INFILE) $(OPTS) -L -b 64 -e floating-point sr_float64_le.wav
	sox $(INFILE) $(OPTS) -e mu-law sr_mu_law.wav
	sox $(INFILE) $(OPTS) -e a-law  sr_a_law.wav
