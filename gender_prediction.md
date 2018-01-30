Lesara Client Gender Prediction
================
Fengchong Wang
January 30, 2018

Data
----

Read data and have and check

``` r
train = fread(paste0(data_dir,'train.csv'), sep = ',')
test = fread(paste0(data_dir,'test.csv'), sep = ',')
```

### First look at the data

``` r
head(train)
```

    ##                                                   client_id     host_name
    ## 1: ef90dd347a4d9c6ab54b2260c0ead5c52ebe480982aadc557a6bc92b www.lesara.it
    ## 2: a2b59f5b6e46b21d9a3c73c10ded2c26e705362190d0f5989f68756c www.lesara.it
    ## 3: d2f62a3ec51b100ea6e7338247e1820a6cba30b2ed1e48bf04f1136c www.lesara.it
    ## 4: a25bc46af1f58744ea4cc210ad39e3b48c78e632de468a5351a5172c www.lesara.it
    ## 5: 4c414b0337a69934083023a82326883f3ba8bfd385d7037be2dd242c www.lesara.it
    ## 6: e6d3fcc2b4295b4fe8f374e006dab57ac4c8fb6583d355350cf37169 www.lesara.it
    ##                                                   page_path    click_time
    ## 1: 54fc1f523d80f473504ec21537ea4fdfaca8ae7d151a5cfe42515d36 1502970344047
    ## 2: 8a5d4b53854e81fb2b6086d3df16f1ec07f4160a99c17220ab889426 1502970344529
    ## 3: cd0ef75ebe8be4785e8ab68b029ee54e376700c39184057b757e08b5 1502970345265
    ## 4: 3f74d8ea5206f388aceaf6ff6a3d9111a1ade37fccb9380114a1d206 1502970346856
    ## 5: a82cd1fe6e31fe07f238d202395c347dc60a6172d2bbfe594cf5ef5a 1502970348018
    ## 6: d92a2a5b993a864ff0233434ede1bb1fa07fe5200707117cae7df380 1502970350337
    ##    gender
    ## 1:      2
    ## 2:      2
    ## 3:      2
    ## 4:      2
    ## 5:      1
    ## 6:      2

### How many different host\_names?

``` r
train[,unique(host_name)]
```

    ## [1] "www.lesara.it"    "www.lesara.co.uk"

### Any duplicated client in training data?

``` r
length(train[,unique(client_id)])/nrow(train)
```

    ## [1] 0.03153133

Yes, only 0.03 unique clients, meaning if each client visits with the same frequency (temporarily wild assumption), every client has 1/0.03153133 = 32 records in training data. Engineered row number will be reduced to around 0.03 of the original training data row number (good!).

### Any leakage in test judged by client\_id?

``` r
any(test[,client_id] %in% train[,client_id])
```

    ## [1] FALSE

Unfortunately, no.

### Any duplicated client in test data?

``` r
length(test[,unique(client_id)])/nrow(test)
```

    ## [1] 0.05284137

Yes, 95% repeated clients - means a smaller test set in fact.

### How many distinct page\_paths in train?

``` r
train[,length(unique(page_path))]
```

    ## [1] 54374

### Any new page\_path in test?

``` r
all(test[,page_path] %in% train[,page_path])
```

    ## [1] FALSE

Unfortunately, there is new page\_path in test.

### Percentage of new page\_path in test?

``` r
1 - sum(test[,page_path] %in% train[,page_path])/nrow(test)
```

    ## [1] 0.01756264

~1.8% new pages in test

### How many new pages in test? How many distince new pages?

``` r
nrow(test[!(test[,page_path] %in% train[,page_path])])
```

    ## [1] 13046

``` r
length(test[!(test[,page_path] %in% train[,page_path]),unique(page_path)])
```

    ## [1] 6061

So within ~13K new pages, ~6K unique.

### Percentage of distinct pages absent from test?

``` r
1 - sum(train[,unique(page_path)] %in% test[,page_path])/54374
```

    ## [1] 0.6335749

63% absent - can remove those pages in training set

Data Engineering
----------------

### One-hot encoding

``` r
train[!(train[,page_path] %in% test[["page_path"]]), page_path := 'removed1']
test[!(test[,page_path] %in% train[["page_path"]]), page_path := 'removed2']
test[, gender := 1]
n_row = nrow(train)
test[, click_time := as.integer64(click_time)]
train = rbind(train, test)

train[, host_name_tmp := FALSE]
train[host_name == "www.lesara.it", host_name_tmp := TRUE]
train[,host_name := host_name_tmp]
train[,host_name_tmp := NULL]

train[, gender := 2 - gender]
```

### Time conversion

``` r
train[,click_time := as.integer(click_time/1000)]
train[host_name==TRUE, click_time_tmp := as.POSIXct(click_time, origin="1970-01-01",tz = 'Europe/Rome')]
train[host_name==FALSE, click_time_tmp := as.POSIXct(click_time, origin="1970-01-01",tz = 'Europe/London')]
train[, click_time := click_time_tmp]
train[, click_time_tmp := NULL]
```

### Shorter name for page\_path

``` r
library(hashmap)
```

    ## 
    ## Attaching package: 'hashmap'

    ## The following object is masked from 'package:bit64':
    ## 
    ##     hashmap

    ## The following object is masked from 'package:bit':
    ## 
    ##     clone

``` r
page_paths = train[,unique(page_path)]
dict = hashmap(keys = page_paths, values = 1:length(page_paths))
remved1_value = dict[['removed1']]
remved2_value = dict[['removed2']]
train[, page_path_tmp := (dict[[train[,(page_path)]]])]
train[, page_path := page_path_tmp]
train[, page_path_tmp := NULL]
```

### Shorter name for client\_id

``` r
client_ids = train[,unique(client_id)]
dict = hashmap(keys = client_ids, values = 1:length(client_ids))
train[, client_id_tmp := (dict[[train[,(client_id)]]])]
train[, client_id := client_id_tmp]
train[, client_id_tmp := NULL]
```

### Click-time related features

``` r
train_tmp1 = train[,paste0(click_time,collapse = ','), by = client_id]
day_of_week = hashmap(keys = c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag","Samstag", "Sonntag"), values = 1:7)

registerDoMC(8)
click_interval = foreach (j = 1:nrow(train_tmp1), .combine = 'c') %dopar% {
  vec = sort(train_tmp1[j,strsplit(V1,',')][,V1], decreasing = FALSE)
  hour = as.integer(names(sort(table(as.integer(gsub(".*\\s+([[:digit:]]+):[[:digit:]]+:[[:digit:]]+", "\\1", vec, perl=TRUE)) + as.integer(round(as.integer(gsub(".*\\s+[[:digit:]]+:([[:digit:]]+):[[:digit:]]+", "\\1", vec, perl=TRUE))/60))), decreasing = TRUE)[1]))
  if (length(vec) > 1) {
    vec = as.POSIXct(vec)
    delta_time = rep(0,length(vec) - 1)
  for (i in 1:(length(vec)-1)) {
    delta_time[i] = as.integer(difftime(vec[i+1], vec[i], units = "secs"))
  }
  c(as.integer(mean(delta_time[delta_time < 60*60])), # average click time interval  (in sec) when two consecutive clicks are within 1 hour
    as.integer(mean(delta_time[delta_time >= 60*60 & delta_time < 60*60*5])/60), # average click time interval (in minute) when two consecutive clicks are within 5 hours
    as.integer(mean(delta_time[delta_time >= 60*60*5])/60/60), # average click time interval  (in hour) when two consecutive clicks are longer than 5 hours
    as.integer(sd(delta_time[delta_time < 60*60])), # sd click time interval when two consecutive clicks are within 1 hour
    as.integer(sd(delta_time[delta_time >= 60*60 & delta_time < 60*60*5])/60), # sd click time interval when two consecutive clicks are within 5 hours
    as.integer(sd(delta_time[delta_time >= 60*60*5])/60/60), # sd click time interval when two consecutive clicks are longer than 5 hours
    as.integer(names(sort(table(day_of_week[[format((as.Date((vec))),'%A')]]),decreasing = TRUE))[1:2]), # most and second frequent visiting day of a week
    hour, # most frequent visiting hour in a day
    length(vec)) # times of clicks
  } else {
    c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, day_of_week[[format(as.Date(as.POSIXct(vec)),'%A')]], day_of_week[[format(as.Date(as.POSIXct(vec)),'%A')]], hour, 1)
  }
}
registerDoSEQ()
features = as.data.table(matrix(data = click_interval, nrow = nrow(train_tmp1), byrow = TRUE))
setnames(features, names(features), c("short_click_avg", "click_avg", "long_click_avg", "short_click_sd", "click_sd", "long_click_sd", "most_freq_day", "second_freq_day", "hour", "n_click"))
# Record whether NA
features[,c('short_na1', 'na1', 'long_na1', 'short_na2', 'na2', 'long_na2') := FALSE]
features[is.na(short_click_avg), short_na1 := TRUE]
features[is.na(click_avg), na1 := TRUE]
features[is.na(long_click_avg), long_na1 := TRUE]
features[is.na(short_click_sd), short_na2 := TRUE]
features[is.na(click_sd), na2 := TRUE]
features[is.na(long_click_sd), long_na2 := TRUE]
# Now infer NA values by mean
m = features[!is.na(short_click_avg),as.integer(mean(short_click_avg))]
features[is.na(short_click_avg), short_click_avg := m]
m = features[!is.na(click_avg),as.integer(mean(click_avg))]
features[is.na(click_avg), click_avg := m]
m = features[!is.na(long_click_avg),as.integer(mean(long_click_avg))]
features[is.na(long_click_avg), long_click_avg := m]
m = features[!is.na(short_click_sd),as.integer(mean(short_click_sd))]
features[is.na(short_click_sd), short_click_sd := m]
m = features[!is.na(click_sd),as.integer(mean(click_sd))]
features[is.na(click_sd), click_sd := m]
m = features[!is.na(long_click_sd),as.integer(mean(long_click_sd))]
features[is.na(long_click_sd), long_click_sd := m]
train_updated = cbind(features, train_tmp1)
train_updated[, V1 := NULL]
rm(train_tmp1)
```

### page\_path related feature engineering

``` r
train_tmp = train[,paste0(ifelse(page_path == remved1_value | page_path == remved2_value, NA_character_, page_path),collapse = ','), by = client_id]
setnames(train_tmp, names(train_tmp), c('client_id','page_path'))
train_tmp[,page_path := gsub(x = page_path, pattern = 'NA,', replacement = '')]
train_tmp[,page_path := gsub(x = page_path, pattern = ',NA$', replacement = '')]
train_updated = merge(train_updated, train_tmp, by = 'client_id')
```

### Merge gender into train\_updated table

``` r
train_tmp = train[,gender, by = client_id]
train_tmp = train_tmp[!duplicated(train_tmp[,client_id])]
train_updated = merge(train_updated, train_tmp, by = 'client_id')
```

### Merge host\_name into train\_updated table

``` r
train_tmp = train[,host_name, by = client_id]
train_tmp = train_tmp[!duplicated(train_tmp[,client_id])]
train_updated = merge(train_updated, train_tmp, by = 'client_id')
train_updated[is.na(second_freq_day), second_freq_day := most_freq_day]
print(train_updated[1])
```

    ##    client_id short_click_avg click_avg long_click_avg short_click_sd
    ## 1:         1              57       108             57            270
    ##    click_sd long_click_sd most_freq_day second_freq_day hour n_click
    ## 1:       26            32             4               3   14     127
    ##    short_na1   na1 long_na1 short_na2   na2 long_na2
    ## 1:     FALSE FALSE    FALSE     FALSE FALSE    FALSE
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           page_path
    ## 1: 1,1,46,64,46,64,93,64,1,136,145,153,64,189,1,224,1,252,264,252,93,1108,18,93,18,35,60,16,23,18,93,1174,1174,1108,896,1108,896,1170,2094,2012,6874,136,145,153,136,285,136,301,527,896,1174,93,1091,1786,4181,1091,93,1091,1174,896,527,1642,1187,1108,285,1642,50,1642,50,285,527,2,50,811,615,331,2367,376,811,35,376,376,156,1821,156,3705,694,7148,4232,6759,7151,35,4232,263,1170,2541,2012,2094,2541,2,615,1187,2094,220,1309,145,153,145,136,1108,1108,1108,1108,2032,2094,2032,896,1174,2577,1108,2577,1174,829,1821,1309
    ##    gender host_name
    ## 1:      0      TRUE

``` r
saveRDS(train_updated, paste0(data_dir, 'engineered_full_data.RDS'))
```

### Convert into libffm compatible format

Field Name -&gt; Field Index

short\_click\_avg -&gt; 1

click\_avg -&gt; 2

long\_click\_avg -&gt; 3

short\_click\_sd -&gt; 4

click\_sd -&gt; 5

long\_click\_sd -&gt; 6

most\_freq\_day -&gt; 7

second\_freq\_day -&gt; 8

hour -&gt; 9

n\_click -&gt; 10

short\_na1 -&gt; 11

na1 -&gt; 12

long\_na1 -&gt; 13

short\_na2 -&gt; 14

na2 -&gt; 15

long\_na2 -&gt; 16

page\_path -&gt; 17

host\_name -&gt; 18

``` r
for (i in 1:nrow(train_updated)) {
  next_line = paste0(train_updated[i, gender],' ')
  for (j in 1:10) { # Numeric features
    next_line = paste0(next_line, paste0(j,':',j,':', train_updated[i, j+1, with = FALSE],' '))
  }
  for (j in c(11:16,18)) { # Binary features
    value = ifelse(train_updated[i, j+1, with = FALSE], 1, 0)
    if (value == 1) {
      next_line = paste0(next_line, paste0(j,':',j,':', value, ' '))
    }
  }
  j = 17 # Categorical features
  if (train_updated[i, j+1,  with = FALSE]$page_path == 'NA') {
    write(paste0(next_line,'\n'), file = paste0(data_dir, 'full.ffm'), append = TRUE, sep = '')
  } else {
    page_path_table = table(strsplit(train_updated[i, j+1,  with = FALSE]$page_path,','))
    write(paste0(next_line,paste0(paste0(as.integer(names(page_path_table))+j+1,':',as.integer(names(page_path_table))+j+1,':', as.vector(page_path_table)), collapse = ' '),'\n'), file = paste0(data_dir, 'full.ffm'), append = TRUE, sep = '')
  }
}
test = fread(paste0(data_dir,'test.csv'), sep = ',')
test_ids = test[,client_id]
test_ids = dict[[test_ids]]
test_rows = train_updated[client_id %in% test_ids, , which =TRUE]
test_rows_test = test_rows - 94595:133846
all(test_rows_test == 0) # So from 94595 in full.ffm file, it belongs to test data
```

### Separate into train, validation and test

Vim was used to do the separation. \``94595gg`, then`SHIFT+v`, then `G`, then `y`, then `:e ./test.ffm`, then `p`, then `:w`, then `ZZ`, then `SHIFT+v`, then `G`, then `d`, then `:w`, then `ZZ`.

Then Bash was used to shuffle the file by `$sort -R full.ffm > train.ffm`

Then `head train.ffm -n 9459 > val.ffm`.

Then `tail -n +9460 train.ffm > subtrain.ffm`

### Training using libffm

In bash, do this `./ffm-train -p ../val.ffm -t 250 ../subtrain.ffm model`

Output looks like this:

    iter   tr_logloss   va_logloss      tr_time
       1      0.38515      0.11574          1.4
       2      0.08085      0.06247          2.6
       3      0.05381      0.04780          3.8
       4      0.04334      0.04028          5.1
       5      0.03737      0.03564          6.4
       6      0.03349      0.03251          7.6
       7      0.03083      0.03028          8.9
    ...
    220      0.01893      0.01966        305.3
     221      0.01895      0.01966        306.9
     222      0.01894      0.01966        308.5
     223      0.01894      0.01966        310.1
     224      0.01893      0.01966        311.7
     225      0.01894      0.01965        313.3
     226      0.01893      0.01965        314.9
     227      0.01893      0.01965        316.5
     228      0.01893      0.01965        318.2
     229      0.01893      0.01965        319.9
     230      0.01893      0.01965        321.5
     231      0.01893      0.01966        323.2
     232      0.01893      0.01966        324.9
     233      0.01893      0.01967        326.6
     234      0.01894      0.01966        328.4
     235      0.01894      0.01966        330.1
     236      0.01894      0.01966        331.9
    ...

So t should be 225 - 230, considering real training data is larger than subtrain file, so choose 230.

`./ffm-train -t 230 ../train.ffm model`

Then remove the faking leading ones in first column: `$ awk 'BEGIN{FS=OFS=" "}{$1="";sub(" ","")}1'  ../test.ffm > ../test.ffm`.

predict by `./ffm-predict ../test.ffm model output.txt`

`output.txt` is a file of size 0, wierd!

There must be some bugs somewhere. No time to debug, too late.

### Try regularized logistic regression without doing the categorical feature engineering

``` r
library(glmnet)
```

    ## Loading required package: Matrix

    ## Loaded glmnet 2.0-13

``` r
fit = cv.glmnet(x = data.matrix(train_updated[1:94594,c(2:17,20), with = FALSE]), y = train_updated[['gender']][1:94594], nfolds = 10, parallel = TRUE)
fit = glmnet(x = data.matrix(train_updated[1:94594,c(2:17,20), with = FALSE]), y = train_updated[['gender']][1:94594], lambda = 0.0003297655)
y_pred = predict(fit, s = 0.0003297655, newx = data.matrix(train_updated[1:94594,c(2:17,20), with = FALSE]))
y_pred[y_pred > 0.5] = 1
y_pred[y_pred <= 0.5] = 0
head(cbind(y_pred,train_updated[['gender']]))
```

    ## Warning in cbind(y_pred, train_updated[["gender"]]): number of rows of
    ## result is not a multiple of vector length (arg 2)

    ##      1  
    ## [1,] 0 0
    ## [2,] 0 0
    ## [3,] 0 0
    ## [4,] 0 0
    ## [5,] 0 1
    ## [6,] 0 0

``` r
all(y_pred == 0)
```

    ## [1] TRUE

So regularized logistic regression is too simple and performs poorly for this problem.

If have time, should do:

1.  One-hot encoding for the page feature
2.  Then try glmnet again
3.  Can also try SVM, NaiveBayes, XGboost
4.  Debug libffm
5.  Ensembling models
