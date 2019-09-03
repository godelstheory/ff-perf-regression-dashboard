# Databricks notebook source
spark.conf.set('spark.databricks.queryWatchdog.enabled', False)

# COMMAND ----------

def get_ms_query(probe_query, start_date, end_date, debug=False, channel='nightly'):
  ms_query = """
    SELECT 
        client_id, 
        SUBSTRING(app_build_id, 1, 8) as app_build_id,
        {0},
        COUNT(*) as num_client_pings
    FROM
        main_summary
    WHERE
        app_build_id >= '{1}'
        AND app_build_id <= '{2}'        
        AND app_name = 'Firefox'        
        AND normalized_channel = 'nightly'    
  """
  if debug:
    ms_query += "\tAND sample_id = '42'\n"
  elif channel == 'nightly' and not debug:
    ms_query += "\tAND sample_id < '70'\n"
  else:
    ms_query += "\tAND sample_id < '25'\n"
       
  ms_query += 'GROUP BY 1, 2'    
  return ms_query.format(probe_query, start_date, end_date)

# COMMAND ----------

# Histogram (client-level) aggregation specific 

from collections import defaultdict
from pyspark.sql.types import StructType, StructField, MapType, IntegerType, DoubleType
from pyspark.sql.functions import udf, col, struct

def agg_client_hists(*args, **kwargs):
  estimator = kwargs.get('estimator', True)
  agg = []
  for probe in args[0]:
    agg.append(agg_client_probe(probe, estimator=estimator))
  return tuple(agg)

def mean_client_hists(x):
  results = []
  for hist in x:
    s, count = 0, 0 
    for key, value in hist.iteritems():
      s += key*value
      count += value
    result = None if not count else s/float(count) # s/count # unsure if this is a problem? covered by schema?
    results.append(result)
  return tuple(results)

def apply_estimator(hist, estimator):  
  if estimator:
    est = {x: y+0.5 for x, y in hist.iteritems()}    
  else:
    est = hist
  return est

# TODO: subclass this also (change name to postprocess?)
def unit_density(hist):
  weight = float(sum(hist.values()))
  return {x: y/weight for x, y in hist.iteritems()}

# TODO: Convert to class with this method subclassed: returns unit hist, means, quantiles, etc...
def agg_client_probe(hists, estimator):
  # sum ping histogram bins
  try:
    agg_hist = defaultdict(int)
    # looping through a single probe list of histograms
    for hist in hists:
      for key, value in hist.iteritems():
        agg_hist[key] += value    
  except Exception:
    agg_hist = defaultdict(int)
  # Apply the Bayesian estimator (remove zeros)
  hist_v2 = apply_estimator(agg_hist, estimator)
  # normalize to unit density
  return unit_density(hist_v2)

# COMMAND ----------

# Poisson Bootstrap Specific

def get_ws(partition, num_bs_reps):     
    for profile in partition:
      # Generate poisson weights for each boostrap replicate
      p_weights = poisson(1, num_bs_reps).tolist()   
      # append a rep to calculate measured RelDS (i.e., w=1 for each profile)
      weights = [1]+p_weights      
      yield ((profile.client_id, profile.app_build_id), {'weights': weights, 'hists': profile.hist_agg})

def weight_hists(profile, probes):
  key, value = profile
  app_build_id = key[1]
  for i, w in enumerate(value['weights']):
    if w > 0:
      result = {
        'n': w,
        'hists': {x: weight_hist(w, getattr(value['hists'], x)) for x in probes}
      }    
      yield ((i, app_build_id), result)  

def weight_hist(w, hist):
  return {x:w*y for x, y in hist.iteritems()}

def sum_hists(a, b, probes):
  results = {
    'hists': {},
    'n': a.get('n', 0) + b.get('n', 0)
  } 
  for probe in probes:
    results['hists'][probe] = sum_hist(a.get('hists', {}), b.get('hists', {}), probe)
  return results

def sum_hist(a, b, probe):  
  hist_a, hist_b = a.get(probe, {}), b.get(probe, {})
  result = {}
  for key in set(hist_a.keys()+hist_b.keys()):
    result[key] = hist_a.get(key, 0)+hist_b.get(key, 0)
  return result

def norm_hists(a): 
  key, value = a
  hists = {}
  for probe, hist in value['hists'].iteritems():
    hists[probe] = unit_density(hist)  
  return (key, hists)

# COMMAND ----------

# CRT specific

def get_comps(builds):
    comps = {}
    num_comps = len(builds)
    for i in range(num_comps):
      result = []
      if i != (num_comps-1):
        result.append((i, 'test'))
      if i != 0:
        result.append((i-1, 'control'))
      comps[builds[i]] = result
    return comps
  
def set_comp(a, comps):
  key, value = a
  app_build_id = key[1]
  for comp in comps[app_build_id]:
    yield (comp[0], key[0]), (app_build_id, comp[1], value)
  
def calc_relds(a, b):    
  assert b[1] != a[1]  
  hists = [a[2], b[2]]
  c_idx = 0 if a[1]=='control' else 1
  controls = hists.pop(c_idx)
  tests = hists[0]
  app_build_id = a[0] if a[1]=='test' else b[0]
  return (calc_relds_stat(controls, tests), app_build_id)

def calc_relds_stat(controls, tests, zero_val = 0.10):
  results = {}
  for probe, control in controls.iteritems():
    test = tests.get(probe, {})
    crr = 0.0
    for key in control:      
      p_c = control[key]
      p_t = test.get(key, 0.0)
      if (p_c == 0):
        if (p_t == 0):
          continue
        else: # Add a big value to denote strangeness
          crr += zero_val
      else:
        crr += abs(((p_c-p_t)/p_c))*((p_c)/1.0)
    results[probe] = crr
  return results

def finalize(a):
  key, value = a
  results = value[0]
  results['rep'] = key[1]
  results['app_build_id'] = value[1]
  return Row(**results)

# COMMAND ----------

# Helper to retrieve the actual histograms for each build

# COMMAND ----------

def hist_bts_to_json(a):
  key, value = a
  value['app_build_id'] = key[1]  
  return value

# COMMAND ----------

# Calculate client means

# COMMAND ----------

def client_means(df, probes):
    schema = StructType([StructField(x, DoubleType(), False) for x in probes])
    mean_hist_udf = udf(mean_client_hists,  schema)
    df_mean = df.withColumn('hist_mean', mean_hist_udf('hist_agg')).select('client_id', 'app_build_id', 'hist_mean')
    # flatten StructType
    df_mean.createOrReplaceTempView('df_mean')
    query = """
    SELECT
      client_id,
      app_build_id,
      {}
    FROM df_mean
    """.format(', '.join('hist_mean["{0}"] as {0}'.format(x) for x in probes))

    return spark.sql(query) 

# COMMAND ----------

# Massage fully aggregate build histograms into DataFrame

# COMMAND ----------

import pandas as pd

def explode_probe(x, probes):
  app_build_id, pr, metric, measure = [], [], [], []  
  for probe in probes:    
    for key, value in x[probe].iteritems():   
      app_build_id.append(x['app_build_id'])
      pr.append(probe)
      metric.append(key)
      measure.append(value)
  return pd.DataFrame({'app_build_id': app_build_id, 'probe': pr, 'metric': metric, 'measure': measure})

def explode_jsons(jsons, probes):
  return pd.concat([explode_probe(row, probes) for row in jsons])

# COMMAND ----------

from datetime import datetime as dt, timedelta
from numpy.random import poisson, binomial
from pyspark.sql import Row
from functools import partial
import pyspark.sql.functions as f
import pandas as pd

def apply_crt(probe_ms_map, final_date, num_days, num_bs_reps=1000, debug=False, channel='nightly',
              estimator=True):
  probes = probe_ms_map.values()
  start_date = dt.strftime(final_date - timedelta(num_days), "%Y%m%d")
  end_date = dt.strftime(final_date, "%Y%m%d")
  # retrieve all probe data for the week 
  probe_query = ',\n\t'.join(['COLLECT_LIST({0}) as {0}'.format(x) for x in probe_ms_map.values()])
  df = spark.sql(get_ms_query(probe_query, start_date, end_date, debug=debug, channel=channel))
  # aggregate the histograms by client
  schema = StructType([StructField(x, MapType(IntegerType(), DoubleType()), False) for x in probes])
  a_client_hists = partial(agg_client_hists, estimator=estimator)
  agg_hist_udf = udf(a_client_hists,  schema)
  df = df.withColumn('hist_agg', agg_hist_udf(struct(*probes)))
  df.cache()
  # get build stats: # of profiles, # of pings 
  names = ['app_build_id', 'num_profiles', 'build_pings']
  build_stats = (df.groupby('app_build_id')
                 .agg(f.count('num_client_pings'), f.sum('num_client_pings'))
                 .toDF(*names))
  build_stats_df = pd.DataFrame(build_stats.collect(), columns=build_stats.columns)
  #### Blows up driver (4G limit) ####
  # extract build meta-data 
#   df.createOrReplaceTempView('df')
#   build_stats = spark.sql("""
#     SELECT 
#       app_build_id,
#       COUNT(*) as num_profiles,
#       SUM(num_client_pings) as build_pings
#     FROM df
#     GROUP BY 1
#   """).toPandas() # FIXME: Blowing things up 
  
  # calculate client means
  df_mean = client_means(df, probes)
  # Generate poisson bootstrap replicates
  g_ws = partial(get_ws, num_bs_reps=num_bs_reps)
  w_hist = partial(weight_hists, probes=probes)
  s_hists = partial(sum_hists, probes=probes)
  hist_bts = df.rdd.mapPartitions(g_ws).flatMap(w_hist).reduceByKey(s_hists).map(norm_hists) 
  # retrieve actual histograms for each build
  hists_actual_jsons = [hist_bts_to_json(x) for x in hist_bts.filter(lambda x: x[0][0]==1).collect()]
  hists_exp = explode_jsons(hists_actual_jsons, probes)
  # Calculate RelDS  
  #### Also blows things up ####
  # builds = sorted([x.app_build_id for x in df.select('app_build_id').distinct().collect()], reverse=True)
  builds = build_stats_df['app_build_id'].sort_values(ascending=False).tolist()
  s_comp = partial(set_comp, comps=get_comps(builds))
  final = hist_bts.flatMap(s_comp).reduceByKey(calc_relds).map(finalize).toDF()
  # split out measured relds from bootstraps
  relds_bts = final.filter(final['rep'] != 1)
  relds_actual = final.filter(final['rep'] == 1)
  return relds_bts, relds_actual, hists_exp, df_mean, build_stats_df