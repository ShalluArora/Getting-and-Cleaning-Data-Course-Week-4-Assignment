# Tidy data set (tidy_data.txt) description

This data set contains mean of different features being observed for 30 subjects performing 6 activities.

### The variables in the tidy_data

Tidy data contains 180 rows and 81 columns. Each row has mean value of different features being observed for each subject and each activity.


### The data is averaged based on subject and activity group.

subject column contains value from 1 to 30 sequentially.
activity_labels column has 6 types as listed below.
1. WALKING
2. WALKING_UPSTAIRS
3. WALKING_DOWNSTAIRS
4. SITTING
5. STANDING
6. LAYING


### The tidy data contains 6 rows for each subject and mean has been calculated for remaining 79 columns based on column activity_labels.
1                          subject
2                  activity_labels
3                tBodyAcc.mean...X
4                tBodyAcc.mean...Y
5                tBodyAcc.mean...Z
6                 tBodyAcc.std...X
7                 tBodyAcc.std...Y
8                 tBodyAcc.std...Z
9             tGravityAcc.mean...X
10            tGravityAcc.mean...Y
11            tGravityAcc.mean...Z
12             tGravityAcc.std...X
13             tGravityAcc.std...Y
14             tGravityAcc.std...Z
15           tBodyAccJerk.mean...X
16           tBodyAccJerk.mean...Y
17           tBodyAccJerk.mean...Z
18            tBodyAccJerk.std...X
19            tBodyAccJerk.std...Y
20            tBodyAccJerk.std...Z
21              tBodyGyro.mean...X
22              tBodyGyro.mean...Y
23              tBodyGyro.mean...Z
24               tBodyGyro.std...X
25               tBodyGyro.std...Y
26               tBodyGyro.std...Z
27          tBodyGyroJerk.mean...X
28          tBodyGyroJerk.mean...Y
29          tBodyGyroJerk.mean...Z
30           tBodyGyroJerk.std...X
31           tBodyGyroJerk.std...Y
32           tBodyGyroJerk.std...Z
33              tBodyAccMag.mean..
34               tBodyAccMag.std..
35           tGravityAccMag.mean..
36            tGravityAccMag.std..
37          tBodyAccJerkMag.mean..
38           tBodyAccJerkMag.std..
39             tBodyGyroMag.mean..
40              tBodyGyroMag.std..
41         tBodyGyroJerkMag.mean..
42          tBodyGyroJerkMag.std..
43               fBodyAcc.mean...X
44               fBodyAcc.mean...Y
45               fBodyAcc.mean...Z
46                fBodyAcc.std...X
47                fBodyAcc.std...Y
48                fBodyAcc.std...Z
49           fBodyAcc.meanFreq...X
50           fBodyAcc.meanFreq...Y
51           fBodyAcc.meanFreq...Z
52           fBodyAccJerk.mean...X
53           fBodyAccJerk.mean...Y
54           fBodyAccJerk.mean...Z
55            fBodyAccJerk.std...X
56            fBodyAccJerk.std...Y
57            fBodyAccJerk.std...Z
58       fBodyAccJerk.meanFreq...X
59       fBodyAccJerk.meanFreq...Y
60       fBodyAccJerk.meanFreq...Z
61              fBodyGyro.mean...X
62              fBodyGyro.mean...Y
63              fBodyGyro.mean...Z
64               fBodyGyro.std...X
65               fBodyGyro.std...Y
66               fBodyGyro.std...Z
67          fBodyGyro.meanFreq...X
68          fBodyGyro.meanFreq...Y
69          fBodyGyro.meanFreq...Z
70              fBodyAccMag.mean..
71               fBodyAccMag.std..
72          fBodyAccMag.meanFreq..
73      fBodyBodyAccJerkMag.mean..
74       fBodyBodyAccJerkMag.std..
75  fBodyBodyAccJerkMag.meanFreq..
76         fBodyBodyGyroMag.mean..
77          fBodyBodyGyroMag.std..
78     fBodyBodyGyroMag.meanFreq..
79     fBodyBodyGyroJerkMag.mean..
80      fBodyBodyGyroJerkMag.std..
81 fBodyBodyGyroJerkMag.meanFreq..

### variable units
subject variable is integer type
activity_labels variable is factor type.
All the other variables are numeric type.

### The original data has around 561 features being observed and but only all the features containing mean and std in their names are kept to create tidy_data (a tidy data set).

* mean: Mean value
* std: Standard deviation