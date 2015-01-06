select 
  %%DICTKEY%% as dictkey, 
  count(*) as dictkeycount 
from 
  (%%QUERY%%) 
group by %%DICTKEY%%
