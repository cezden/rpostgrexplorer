-- based on http://golden13.blogspot.com/2012/08/how-to-get-some-information-about.html
SELECT 
  n.nspname as schema, 
	c.relname as objname, 
	CASE c.relkind 
		WHEN 'r' THEN 'table'  
		WHEN 'v' THEN 'view' 
		WHEN 'i' THEN 'index' 
		WHEN 'S' THEN 'sequence'  
		WHEN 's' THEN 'special' 
		END as objtype, 
	u.usename as objowner,
  (SELECT obj_description(c.oid, 'pg_class')) AS comment  
FROM 
	pg_catalog.pg_class c 
	LEFT JOIN pg_catalog.pg_user u ON u.usesysid = c.relowner 
	LEFT JOIN pg_catalog.pg_namespace n ON n.oid = c.relnamespace 
WHERE 
	n.nspname NOT IN ('pg_catalog', 'pg_toast', 'information_schema')
  %%SCHEMA_SELECTOR%%
ORDER BY objname ASC
