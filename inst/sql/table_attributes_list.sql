select 
  a.*, 
  b.null_frac, b.n_distinct, b.most_common_vals, b.most_common_freqs, b.histogram_bounds 
from 
  (select 
      t.schemaname, t.tablename,
      a.attname,
      tp.typname,
      a.attlen
  from 
      pg_attribute a, 
      pg_class c, 
      pg_tables t, 
      pg_namespace nsp, 
      pg_type tp
  where 
      a.attrelid = c.oid and 
      c.relname = t.tablename and 
      c.relnamespace = nsp.oid and 
      a.attnum > 0 and 
      tp.oid=a.atttypid and 
      nsp.nspname = t.schemaname
  ) a
          
    left join 
          
  (select 
      t.schemaname, t.tablename,
      a.attname, 
      pgs.null_frac, pgs.n_distinct, pgs.most_common_vals, pgs.most_common_freqs, pgs.histogram_bounds 
   from 
      pg_attribute a, 
      pg_class c, 
      pg_tables t, 
      pg_namespace nsp, 
      pg_type tp, 
      pg_stats pgs 
    where 
      a.attrelid = c.oid and 
      c.relname = t.tablename and 
      c.relnamespace = nsp.oid and 
      a.attnum > 0 and 
      tp.oid=a.atttypid and 
      nsp.nspname = t.schemaname and
      pgs.schemaname = t.schemaname and 
      pgs.tablename = t.tablename and 
      a.attname = pgs.attname 
    ) b
      
      on (a.tablename = b.tablename and a.attname = b.attname and a.schemaname = b.schemaname)

%%WHERE_SCHEMA_SELECTOR%%

order by 
  schemaname, tablename 