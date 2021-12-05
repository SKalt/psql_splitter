-- test a large nested if using a variety of true-equivalents
\if true
	\if 1
		\if yes
			\if on
				\echo 'all true'
			\else
				\echo 'should not print #1-1'
			\endif
		\else
			\echo 'should not print #1-2'
		\endif
	\else
		\echo 'should not print #1-3'
	\endif
\else
	\echo 'should not print #1-4'
\endif
