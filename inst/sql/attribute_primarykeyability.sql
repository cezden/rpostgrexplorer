select 
        sum(%%VALCOUNTER%%) as row_cnt, 
        count(*) as distinct_val_cnt, 
        min(%%VALCOUNTER%%) as mincnt, 
        avg(%%VALCOUNTER%%) as avgcnt, 
        max(%%VALCOUNTER%%) as maxcnt, 
        stddev_samp(%%VALCOUNTER%%) as sdevcnt,
        sum(%%NULLITYMARKER%% * %%VALCOUNTER%%) as null_cnt 
        from
        (select 
            %%VALNAME%%, 
            count(*) as %%VALCOUNTER%%, 
            (case 
              when %%VALNAME%% is null then 1 
              else 0 end
            ) as %%NULLITYMARKER%% 
          from 
            (%%QUERY%%) 
          group by 
            %%VALNAME%%)
