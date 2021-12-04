-- tests for \if ... \endif

\if true
  select 'okay';
  select 'still okay';
\else
  not okay;
  still not okay
\endif

-- at this point query buffer should still have last valid line
\g
