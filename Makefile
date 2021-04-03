run:
	stack run +RTS -N

prof:
	stack run --profile -- +RTS -N -xc
