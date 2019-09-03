# Databricks notebook source
import pandas as pd

# COMMAND ----------

# MAGIC %md Load up the required functions

# COMMAND ----------

# MAGIC %run ./crt_ms_lib

# COMMAND ----------

# MAGIC %md Parameters for RelDS process

# COMMAND ----------

num_days_backfill = 7 # number of days to process
# num_days = 6 # number of days/builds to apply calculation 
num_bts_rep = 1000 # number of Poisson bootstrap replicates
debug = False # Limit to single sample ID or run the gamut
channel = 'nightly'
estimator = False

# Serialization parameters
data_bucket = "net-mozaws-prod-us-west-2-pipeline-analysis"
s3root = 'cdowhygelund/CRT/V0_21_full/no_est/page_load/{}/'.format(channel)
s3path = s3root+"relds_csv"
s3path_measured = s3root+"relds_measured"
s3path_hists = s3root+"probe_hists"
s3path_means = s3root+"client_means"
output_file = 's3://{}/{}'.format(data_bucket, s3path)
output_measured_file = 's3://{}/{}'.format(data_bucket, s3path_measured)
client_means_file = 's3://{}/{}'.format(data_bucket, s3path_means)

# Probes to apply RelDS
probe_ms_map = {
  'TIME_TO_DOM_COMPLETE_MS': 'histogram_content_time_to_dom_complete_ms',
  'TIME_TO_DOM_CONTENT_LOADED_END_MS': 'histogram_content_time_to_dom_content_loaded_end_ms',
  'TIME_TO_LOAD_EVENT_END_MS': 'histogram_content_time_to_load_event_end_ms',
  'TIME_TO_NON_BLANK_PAINT_MS': 'histogram_content_time_to_non_blank_paint_ms',
  'TIME_TO_NON_BLANK_PAINT_MS_PARENT': 'histogram_parent_time_to_non_blank_paint_ms',
  # 'FX_PAGE_LOAD_MS_2': 'histogram_content_fx_page_load_ms_2',
  'FX_PAGE_LOAD_MS_2_PARENT': 'histogram_parent_fx_page_load_ms_2'
}

# COMMAND ----------

# MAGIC %md UDF to calculate RelDS quantiles and produce final result

# COMMAND ----------

# MAGIC %md Calculate RelDS

# COMMAND ----------

from datetime import datetime as dt

now = dt.now().date()

# Retrieve data, aggregate histograms, bootstrap, calculate RelDS
relds_bts, relds_actual, hists_exp, df_mean, build_stats = apply_crt(probe_ms_map, now, 
                                                                     num_days_backfill, num_bs_reps=1000, 
                                                                     debug=debug, channel=channel, 
                                                                     estimator=estimator)

# COMMAND ----------

# MAGIC %md Calculate RelDS quantiles

# COMMAND ----------

from pyspark.sql.functions import pandas_udf, PandasUDFType, lit
from pyspark.sql.types import StringType, IntegerType, DoubleType

quantiles = [0.05,0.1,0.25, 0.50, 0.75, 0.9,0.95]
relds_cols = ['relds_{}'.format(str(x)[2:]) for x in quantiles]

schema = StructType([StructField('probe', StringType(), False), 
                     StructField('app_build_id', StringType(), False),
                 StructField('num_profiles', IntegerType(), True),
                 StructField('num_pings', IntegerType(), True)] + 
                [StructField(x, DoubleType(), False) for x in relds_cols]
               )

probes = probe_ms_map.values()

@pandas_udf(schema, PandasUDFType.GROUPED_MAP)
def calculate_quantile(pdf):
  result_df = pdf[probes].quantile(quantiles)
  app_build_id = pdf.app_build_id.iloc[0]  
  app_build_df = build_stats[build_stats.app_build_id==app_build_id]
  num_profiles = app_build_df.num_profiles.iloc[0]
  num_pings = app_build_df.build_pings.iloc[0]
  results = []
  for probe in probes:
    result = {'probe':probe, 'app_build_id': app_build_id, 
              'num_profiles': num_profiles, 'num_pings': num_pings}
    result.update({relds_cols[i]: result_df[probe][x] for i,x in enumerate(quantiles)})
    results.append(result)
  final_df = pd.DataFrame(results)
  return final_df[['probe', 'app_build_id', 'num_profiles', 'num_pings'] + relds_cols] 

relds = relds_bts.groupby('app_build_id').apply(calculate_quantile)
relds = relds.withColumn('creation_date', lit(now)) 

# COMMAND ----------

# MAGIC %md Serialize the relevant data

# COMMAND ----------

# MAGIC %md RelDS quantiles

# COMMAND ----------

relds.write.csv(output_file, mode='append', header=True)

# COMMAND ----------

# MAGIC %md Histograms

# COMMAND ----------

from io import BytesIO
import boto3

hists_exp['creation_date'] = now
s3_resource = boto3.resource('s3')
with BytesIO() as f:
  hists_exp.to_csv(f, index=False)
  p_hist_csv = '{}/probe_hists_{}.csv'.format(s3path_hists, now.strftime('%Y%m%d'))
  s3_resource.Object(data_bucket, p_hist_csv).put(Body=f.getvalue())    

# COMMAND ----------

# MAGIC %md Measured RelDS

# COMMAND ----------

relds_actual.write.csv(output_measured_file, mode='append')

# COMMAND ----------

# MAGIC %md Client Means

# COMMAND ----------

df_mean.withColumn('creation_date', lit(now)).write.csv(client_means_file, mode='append', 
                                                                 header=True)