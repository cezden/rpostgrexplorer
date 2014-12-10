select 
  t.schemaname, t.tablename, 
  c.relnatts as attributenumber, c.reltuples as count_estimate, c.relhasindex as has_index, c.relhaspkey as has_primary_key
from 
  pg_class c, 
  pg_tables t, 
  pg_namespace nsp
where 
  c.relname = t.tablename and 
  c.relnamespace = nsp.oid and 
  nsp.nspname = t.schemaname 
  %%SCHEMA_SELECTOR%% 
order by 
  schemaname, tablename
