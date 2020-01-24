library(htmlTable)

summary_stats_table <- structure(c("9", "33", "24", "19", "6", #SHP-untreated
                               "13", "60", "47", "26", "9", #SHP-vehicle-treated
                               "9", "60", "51", "24", "9", #SHP_combined
                               "131", "180", "48", "150", "8", #RHP_untreated
                               "135", "180", "46", "154", "8", #RHP_vehicle-treated
                               "131", "180", "49", "151", "8"), #RHP_combined
                             .Dim = c(5L, 6L),
                             .Dimnames = list(NULL, c("Untreated<br />(n = 33)", "Vehicle-<br />treated<br />(n = 75)", "Combined<br />(n = 108)",
                                                      "Untreated<br />(n = 826)", "Vehicle-<br />treated<br />(n = 84)", "Combined<br />(n = 910)")))
rownames(summary_stats_table) <- c("Minimum", "Maximum", "Range", "Mean", "Standard<br />deviation")
rownames(summary_stats_table) <- sprintf('<b>%s</b>', rownames(summary_stats_table))
cgroup <- c("Standard hot plate", "Ramped hot plate")

htmlTable(summary_stats_table, rowlabel = "Summary<br />statistic", 
          ctable = TRUE, align = 'ccccc',
          ## number of columns that each cgroup label spans:
          n.cgroup = c(3, 3), cgroup = cgroup,
          ## insert two table spanning sections:
          
          #           css.tspanner.sep = "border-bottom: 1px dotted grey;",
          caption = "Table 1: Summary statistics for the standard hot plate and ramped hot plate control population.", 
          tfoot = '<font size=3><sup>&dagger;</sup>All values are rounded to the nearest integer.</font>')





normality_table <- structure(c("P<0.0001", "P<0.0001", "59%", #data_column1_
                                         "P = 0.9305<br />Normal", "P = 0.8730<br />Normal ", "39%", #data_column2
                                         "P<0.0001", "P = 0.0002", "39%", #data_column3
                                         "P = 0.0532<br />Normal", "P = 0.0732<br />Normal", "37%"), #data_column4
                                       .Dim = c(3L, 4L),
                                       .Dimnames = list(NULL, c("Untransformed", "Logarithmic-<br />transformation",
                                                                "Untransformed", "Logarithmic-<br />transformation")))
rownames(normality_table) <- c("D'Agostino & Pearson<br />normality test", "Shapiro-Wilk<br />normality test",  "Coefficient of<br />variation<sup>&dagger;</sup>")
rownames(normality_table) <- sprintf('<b>%s</b>', rownames(normality_table))
cgroup <- c("Standard hot plate<br />(n = 108)", "Ramped hot plate<br />(n = 910)")

htmlTable(normality_table, rowlabel = "Measure<br /> ", 
          ctable = TRUE, align = 'ccccc',
          ## number of columns that each cgroup label spans:
          n.cgroup = c(2, 2), cgroup = cgroup,
          ## insert two table spanning sections:
          
          #           css.tspanner.sep = "border-bottom: 1px dotted grey;",
          caption = "Table 2: Assessment of the normality and variance of the control populations under each protocol", 
          tfoot = '<font size=3><sup>&dagger;</sup>The range of response time (below minimum-observed value) for which there were no values was omitted.</font>')




dose_response_table <- structure(c("2.6 mg/kg<br />(1.7-3.5)", "0.8 mg/kg<br />(0.5-1.0)", #data_column1_
                               "2.6<br />(0.0-5.2)", "1.6<br />(0.9-2.3)", #data_column2
                               "5.7 mg/kg<br />(4.4-7.0)", "1.6 mg/kg<br />(1.3-2.0)", #data_column3
                               "2.1<br />(1.1-3.1)", "1.5<br />(1.1-1.9)"), #data_column4
                             .Dim = c(2L, 4L),
                             .Dimnames = list(NULL, c("ED50<br />(95% CI)", "Hillslope<br />(95% CI)",
                                                      "ED50<br />(95% CI)", "Hillslope<br />(95% CI)")))
rownames(dose_response_table) <- c("Morphine", "Pure THC")
rownames(dose_response_table) <- sprintf('<b>%s</b>', rownames(dose_response_table))
cgroup <- c("Standard hot plate<br />protocol", "Ramped hot plate<br />protocol")

htmlTable(dose_response_table, rowlabel = "Reference <br />compound", 
          ctable = TRUE, align = 'ccccc',
          ## number of columns that each cgroup label spans:
          n.cgroup = c(2, 2), cgroup = cgroup,
          ## insert two table spanning sections:
          
          #           css.tspanner.sep = "border-bottom: 1px dotted grey;",
          caption = "Table 3: Estimates on the effective dose and slope with both protocols") 






