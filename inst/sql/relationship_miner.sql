select 
           count(*) as distvals_cnt,

           avg(aone_cnt_ind::float8) as f_distentities_frac,
           avg(atwo_cnt_ind::float8) as s_distentities_frac,
           avg(aone_cnt_ind::float8*atwo_cnt_ind::float8) as distentities_match_frac,

           min(aone_cnt) as f_mincnt, 
           avg(aone_cnt::float8) as f_avgcnt, 
           max(aone_cnt) as f_maxcnt, 
           stddev_samp(aone_cnt::float8) as f_sdevcnt,
           sum(aone_cnt) as f_sumcnt,
           avg(aone_cnt_nullable::float8) as f_avgcnt_when_present,
           stddev_samp(aone_cnt_nullable::float8) as f_sdevcnt_when_present,
           min(aone_cnt_both_present) as f_mincnt_both_present, 
           avg(aone_cnt_both_present::float8) as f_avgcnt_both_present, 
           max(aone_cnt_both_present) as f_maxcnt_both_present, 
           stddev_samp(aone_cnt_both_present::float8) as f_sdevcnt_both_present,

           min(atwo_cnt) as s_mincnt, 
           avg(atwo_cnt::float8) as s_avgcnt, 
           max(atwo_cnt) as s_maxcnt, 
           stddev_samp(atwo_cnt::float8) as s_sdevcnt,
           sum(atwo_cnt) as s_sumcnt,
           avg(atwo_cnt_nullable::float8) as s_avgcnt_when_present,
           stddev_samp(atwo_cnt_nullable::float8) as s_sdevcnt_when_present,
           min(atwo_cnt_both_present) as s_mincnt_both_present, 
           avg(atwo_cnt_both_present::float8) as s_avgcnt_both_present, 
           max(atwo_cnt_both_present) as s_maxcnt_both_present, 
           stddev_samp(atwo_cnt_both_present::float8) as s_sdevcnt_both_present
from
         
      (
          select 
             coalesce(aone.%%Q1_VALCOUNTER%%, 0) as aone_cnt, 
             (case when aone.%%Q1_VALCOUNTER%% is null then 0.0 else 1.0 end) as aone_cnt_ind,
             aone.%%Q1_VALCOUNTER%% as aone_cnt_nullable,
             (case when atwo.%%Q2_VALCOUNTER%% is null then null else aone.%%Q1_VALCOUNTER%% end) as aone_cnt_both_present,
             coalesce(atwo.%%Q2_VALCOUNTER%%, 0) as atwo_cnt, 
             (case when atwo.%%Q2_VALCOUNTER%% is null then 0.0 else 1.0 end) as atwo_cnt_ind,
             atwo.%%Q2_VALCOUNTER%% as atwo_cnt_nullable,
             (case when aone.%%Q1_VALCOUNTER%% is null then null else atwo.%%Q2_VALCOUNTER%% end) as atwo_cnt_both_present
         from
           (select 
              %%Q1_VALNAME%% as eone, 
              count(*) as %%Q1_VALCOUNTER%% 
            from 
                (%%Q1_QUERY%%)
            where 
                %%Q1_VALNAME%% is not null 
            group by %%Q1_VALNAME%%
           ) aone
           
           full outer join
           
           (select 
              %%Q2_VALNAME%% as eone, 
              count(*) as %%Q2_VALCOUNTER%% 
            from 
                (%%Q2_QUERY%%) 
            where 
                %%Q2_VALNAME%% is not null 
            group by 
              %%Q2_VALNAME%%
            ) atwo
           
           on aone.eone=atwo.eone
      )        
