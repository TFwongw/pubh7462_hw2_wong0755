pubh7462\_hw2\_RMD
================
Tsz Fung Wong
January 28,2022

-   [Question 1](#question-1)
-   [Data Description](#data-description)
-   [Q3.3.1](#q331)
-   [Q3.3.2](#q332)
-   [Q3.3.3](#q333)
-   [Q3.3.4](#q334)

# Question 1

``` r
brfss = read_csv("./data/brfss_smart_2010.csv")
brfss_df = as_tibble(brfss) %>%
  janitor::clean_names() %>% #clean names
  filter(topic == "Overall Health") %>%
  select(c("year", "locationabbr", "locationdesc", "response", 
             "sample_size", "response", "data_value")) %>%
    mutate(county = stringr::str_remove(locationdesc, str_flatten(str_c(unique(locationabbr), " - "), "|"))) %>%
  select(-"locationdesc") %>%
  rename("state" = "locationabbr", 
           "prop_of_response" = "data_value") 
brfss_df$response = fct_relevel(factor(brfss_df$response), "Excellent", "Very good", "Good", "Fair")
brfss_df$county = factor(brfss_df$county)
brfss_df$state = factor(brfss_df$state)
```

# Data Description

The modified BRFSS data consist of 10625 observations described with 6
variables measuring overall health conditions. The first variable year
indicate the time of data collection, ranging from 2002 to 2010.
Variable state and county indicate the geographical information of the
observation from 51 states. Response variable indicate the 5 type of
response indicating overall health conditions. Sample\_size variable
indicate the amount of response collected from the specific county for
different type of response. Prop\_of response indicate the proportion of
response accociated with the corresponding type of response.

# Q3.3.1

In year 2004, the states that were observed at 6 locations are AZ, GA,
ID, ME, MI, VT.

# Q3.3.2

``` r
brfss_plot = brfss_df %>%
  filter(year >= '2002' & year <= '2010') %>%
  group_by(year, state) %>%
  summarise(num_location = n_distinct(county)) %>%
  mutate(
    year = year %>%
           as.character() %>%
           as.numeric()
  ) 

brfss_plot %>%
  ggplot(aes(x = year, y = num_location, colour = fct_reorder2(state, year, num_location))) +
  geom_line(size = 1, alpha = 0.44) +
  stat_smooth(alpha = 0.1, 
              size = 0.2, 
              method = "loess",
              span = 2,
              se = FALSE) +
  labs(
    x = "Year",
    y = "Number of Observed Locations",
    title = "Number of Observed Locations by State from 2002 - 2010"
  ) +
  scale_colour_viridis_d("State") +
  theme(legend.position = "right", 
        legend.key.size = unit(0.2, "cm"),
        legend.key.height = unit(0.2, 'cm'),
         legend.key.width = unit(0.05, 'cm'))
```

<img src="BRFSS_RMD_files/figure-gfm/unnamed-chunk-2-1.png" width="90%" style="display: block; margin: auto;" />
State NJ has the highest mean number of locations. Most state has low
number of locations observed over time, whereas state FL has two peaks
only in year 2007 and 2010.

# Q3.3.3

``` r
brfss_sum = brfss_df %>%
  filter(year %in% c("2002", "2010", "2006")) %>%
  filter(state %in% c("MN")) %>%
  filter(response %in% c("Excellent", "Good", "Poor")) %>% 
  group_by(year, response) %>%
  summarise(
    across(
      contains(c("sample", "prop")), 
      list(mean = mean, sd = sd), na.rm=TRUE, 
      .names = "{.col}_{.fn}"
    )
  )

brfss_sum  %>%
  gt() %>%
  tab_header("Summary of Sample Size and Proportion of Response")
```

<div id="uvhjkozlsy" style="overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#uvhjkozlsy .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#uvhjkozlsy .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uvhjkozlsy .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#uvhjkozlsy .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#uvhjkozlsy .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uvhjkozlsy .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#uvhjkozlsy .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#uvhjkozlsy .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#uvhjkozlsy .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#uvhjkozlsy .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#uvhjkozlsy .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#uvhjkozlsy .gt_group_heading {
  padding: 8px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
}

#uvhjkozlsy .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#uvhjkozlsy .gt_from_md > :first-child {
  margin-top: 0;
}

#uvhjkozlsy .gt_from_md > :last-child {
  margin-bottom: 0;
}

#uvhjkozlsy .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#uvhjkozlsy .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 12px;
}

#uvhjkozlsy .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uvhjkozlsy .gt_first_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
}

#uvhjkozlsy .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#uvhjkozlsy .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#uvhjkozlsy .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#uvhjkozlsy .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#uvhjkozlsy .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uvhjkozlsy .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding: 4px;
}

#uvhjkozlsy .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#uvhjkozlsy .gt_sourcenote {
  font-size: 90%;
  padding: 4px;
}

#uvhjkozlsy .gt_left {
  text-align: left;
}

#uvhjkozlsy .gt_center {
  text-align: center;
}

#uvhjkozlsy .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#uvhjkozlsy .gt_font_normal {
  font-weight: normal;
}

#uvhjkozlsy .gt_font_bold {
  font-weight: bold;
}

#uvhjkozlsy .gt_font_italic {
  font-style: italic;
}

#uvhjkozlsy .gt_super {
  font-size: 65%;
}

#uvhjkozlsy .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 65%;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <th colspan="5" class="gt_heading gt_title gt_font_normal gt_bottom_border" style>Summary of Sample Size and Proportion of Response</th>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1">response</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sample_size_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">sample_size_sd</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">prop_of_response_mean</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_right" rowspan="1" colspan="1">prop_of_response_sd</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2002</td>
    </tr>
    <tr><td class="gt_row gt_center">Excellent</td>
<td class="gt_row gt_right">116.00</td>
<td class="gt_row gt_right">83.275</td>
<td class="gt_row gt_right">24.15</td>
<td class="gt_row gt_right">3.5407</td></tr>
    <tr><td class="gt_row gt_center">Good</td>
<td class="gt_row gt_right">123.75</td>
<td class="gt_row gt_right">84.263</td>
<td class="gt_row gt_right">23.95</td>
<td class="gt_row gt_right">1.0472</td></tr>
    <tr><td class="gt_row gt_center">Poor</td>
<td class="gt_row gt_right">13.75</td>
<td class="gt_row gt_right">9.570</td>
<td class="gt_row gt_right">2.40</td>
<td class="gt_row gt_right">1.1690</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2006</td>
    </tr>
    <tr><td class="gt_row gt_center">Excellent</td>
<td class="gt_row gt_right">122.33</td>
<td class="gt_row gt_right">72.625</td>
<td class="gt_row gt_right">23.83</td>
<td class="gt_row gt_right">2.9872</td></tr>
    <tr><td class="gt_row gt_center">Good</td>
<td class="gt_row gt_right">137.33</td>
<td class="gt_row gt_right">85.816</td>
<td class="gt_row gt_right">26.37</td>
<td class="gt_row gt_right">0.4509</td></tr>
    <tr><td class="gt_row gt_center">Poor</td>
<td class="gt_row gt_right">15.00</td>
<td class="gt_row gt_right">6.928</td>
<td class="gt_row gt_right">2.30</td>
<td class="gt_row gt_right">0.9539</td></tr>
    <tr class="gt_group_heading_row">
      <td colspan="5" class="gt_group_heading">2010</td>
    </tr>
    <tr><td class="gt_row gt_center">Excellent</td>
<td class="gt_row gt_right">203.80</td>
<td class="gt_row gt_right">190.598</td>
<td class="gt_row gt_right">25.44</td>
<td class="gt_row gt_right">5.2776</td></tr>
    <tr><td class="gt_row gt_center">Good</td>
<td class="gt_row gt_right">220.00</td>
<td class="gt_row gt_right">196.099</td>
<td class="gt_row gt_right">26.04</td>
<td class="gt_row gt_right">3.5473</td></tr>
    <tr><td class="gt_row gt_center">Poor</td>
<td class="gt_row gt_right">27.40</td>
<td class="gt_row gt_right">27.318</td>
<td class="gt_row gt_right">2.36</td>
<td class="gt_row gt_right">0.7701</td></tr>
  </tbody>
  
  
</table>
</div>

More above average overall health people are sampled, and they tends to
have higher response rate than the people with poor health condition.
The number of people sampled increase by year generally but the
proportion of people respond tends to be consistent throughout the three
year.

# Q3.3.4

Response Trend of MN

``` r
brfss_sum %>% 
  pivot_longer(-c(year, response),
  names_to = "type",
  values_to = "value"
  ) %>%
  ggplot(aes(x = year, y = value, colour = response)) +
  geom_point(size = 4, alpha = 0.44, shape = 16) +
  stat_smooth(alpha = 0.1, 
              size = 1.2, 
              method = "loess",
              span = 2,
              se = FALSE) +
  labs(
    x = "Year",
    y = "Density",
    title = "Trend of Response Portion and Sample Size by Response Type"
  ) +
  scale_colour_viridis_d("Response") +
  facet_wrap(~ type, scales = "free", ncol = 2) + 
  scale_x_continuous(breaks = c(2002, 2006, 2010))
```

<img src="BRFSS_RMD_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" style="display: block; margin: auto;" />
