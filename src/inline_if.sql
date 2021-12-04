-- \if should work okay on part of a query
select
  \if true
    42
  \else
    (bogus
  \endif
  forty_two;
