GRANT SELECT ON TABLE rls_t1 TO regress_rls_copy_user;
GRANT SELECT (a, b) ON TABLE rls_t1 TO regress_rls_copy_user_colperms;

-- all columns
COPY rls_t1 TO stdout;
COPY rls_t1 (a, b, c) TO stdout;

-- subset of columns
COPY rls_t1 (a) TO stdout;
COPY rls_t1 (a, b) TO stdout;

-- column reordering
COPY rls_t1 (b, a) TO stdout;

SET SESSION AUTHORIZATION regress_rls_copy_user;

-- all columns
COPY rls_t1 TO stdout;
COPY rls_t1 (a, b, c) TO stdout;

-- subset of columns
COPY rls_t1 (a) TO stdout;
COPY rls_t1 (a, b) TO stdout;

-- column reordering
COPY rls_t1 (b, a) TO stdout;

RESET SESSION AUTHORIZATION;

SET SESSION AUTHORIZATION regress_rls_copy_user_colperms;

-- attempt all columns (should fail)
COPY rls_t1 TO stdout;
COPY rls_t1 (a, b, c) TO stdout;

-- try to copy column with no privileges (should fail)
COPY rls_t1 (c) TO stdout;

-- subset of columns (should succeed)
COPY rls_t1 (a) TO stdout;
COPY rls_t1 (a, b) TO stdout;

RESET SESSION AUTHORIZATION;

-- test with INSTEAD OF INSERT trigger on a view
CREATE TABLE instead_of_insert_tbl(id serial, name text);
CREATE VIEW instead_of_insert_tbl_view AS SELECT ''::text AS str;

COPY instead_of_insert_tbl_view FROM stdin; -- fail
test1
\.
