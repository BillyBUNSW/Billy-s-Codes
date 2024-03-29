library(tidyr)
library('tidyverse')
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library('cowplot')
library('patchwork')
library('gridExtra')
library('FactoMineR')
library(lubridate)
library('plotly')
library(leaps)
library(caret)
library(car)
library(pROC)
library(smotefamily)
library(glmnet)
library("PRROC")
library("class")
library(imbalance)
library(UBL)
library(forcats)



df <- read.csv("VicRoadFatalData.csv", stringsAsFactors = TRUE)
summary(df)
str(df)
# Refining the data
add_variables <- function(df) {
  # Redefining postcodes by Region
  df$REGION <- ifelse(df$OWNER_POSTCODE %in% c(3350, 3355, 3356, 3358, 3353, 3354), "Ballarat", 
                      ifelse(df$OWNER_POSTCODE %in% c(3214, 3215, 3216, 3219, 3220), "Geelong", 
                             ifelse(df$OWNER_POSTCODE %in% c(3550, 3552, 3555, 3556, 3005, 3010, 3050, 3086, 3138, 3198, 3199, 3200, 3201, 3800, 3805, 3910, 3930, 3116, 3140, 3338, 3340, 3429, 3806, 3807, 3809, 3810, 3911, 3913, 3915, 3931, 3934, 3938, 3940, 3941, 3942, 3943, 3944, 3024, 3089, 3091, 3096, 3097, 3099, 3115, 3154, 3158, 3159, 3160, 3164, 3765, 3793, 3796, 3804, 3977, 3100, 3336, 3339, 3436, 3439, 3443, 3768, 3769, 3771, 3772, 3773, 3774, 3776, 3780, 3784, 3790, 3794, 3798, 3811, 3914, 3917, 3932, 3935, 3982, 3983), "Melbourne",
                                    ifelse(df$OWNER_POSTCODE %in% c(3000, 3002, 3003, 3004, 3006, 3008, 3011, 3012, 3013, 3015, 3016, 3018, 3019, 3020, 3021, 3022, 3023, 3025, 3026, 3027, 3028, 3029, 3030, 3031, 3032, 3033, 3034, 3036, 3037, 3038, 3039, 3040, 3041, 3042, 3043, 3044, 3045, 3046, 3047, 3048, 3049, 3051, 3052, 3053, 3054, 3055, 3056, 3057, 3058, 3059, 3060, 3061, 3062, 3063, 3064, 3065, 3066, 3067, 3068, 3070, 3071, 3072, 3073, 3074, 3075, 3076, 3078, 3079, 3081, 3082, 3083, 3084, 3085, 3087, 3088, 3090, 3093, 3094, 3095, 3101, 3102, 3103, 3104, 3105, 3106, 3107, 3108, 3109, 3111, 3113, 3114, 3121, 3122, 3123, 3124, 3125, 3126, 3127, 3128, 3129, 3130, 3131, 3132, 3133, 3134, 3135, 3136, 3137, 3141, 3142, 3143, 3144, 3145, 3146, 3147, 3148, 3149, 3150, 3151, 3152, 3153, 3155, 3156, 3161, 3162, 3163, 3165, 3166, 3167, 3168, 3169, 3170, 3171, 3172, 3173, 3174, 3175, 3177, 3178, 3179, 3180, 3181, 3182, 3183, 3184, 3185, 3186, 3187, 3188, 3189, 3190, 3191, 3192, 3193, 3194, 3195, 3196, 3197, 3202, 3204, 3205, 3206, 3207, 3428, 3752, 3802, 3803, 3975, 3976, 3001, 3007, 3009, 3014, 3017, 3035, 3069, 3077, 3080, 3092, 3098, 3110, 3112, 3117, 3118, 3119, 3120, 3157, 3176, 3203, 3208, 3209, 3210, 3801), "Melbourne Metro",
                                           ifelse(df$OWNER_POSTCODE %in% c(3218, 3224, 3912, 3936, 3939, 3551, 3139, 3211, 3212, 3213, 3217, 3221, 3222, 3223, 3225, 3226, 3227, 3228, 3230, 3231, 3232, 3335, 3337, 3341, 3342, 3345, 3351, 3352, 3357, 3360, 3361, 3427, 3430, 3431, 3432, 3433, 3434, 3435, 3437, 3438, 3440, 3441, 3442, 3444, 3446, 3447, 3448, 3450, 3451, 3453, 3458, 3460, 3461, 3693, 3750, 3751, 3753, 3754, 3755, 3756, 3757, 3758, 3759, 3760, 3761, 3762, 3763, 3764, 3766, 3767, 3770, 3775, 3777, 3778, 3779, 3781, 3782, 3783, 3785, 3786, 3787, 3788, 3789, 3791, 3792, 3795, 3797, 3799, 3808, 3812, 3813, 3814, 3815, 3816, 3818, 3916, 3918, 3919, 3920, 3921, 3922, 3923, 3925, 3926, 3927, 3928, 3929, 3933, 3937, 3945, 3946, 3950, 3951, 3978, 3979, 3980, 3981, 3984, 3987, 3988, 3990, 3991, 3992, 3995, 3996, 3691, 3233, 3234, 3235, 3236, 3237, 3238, 3239, 3240, 3241, 3242, 3243, 3249, 3250, 3251, 3254, 3260, 3264, 3265, 3266, 3267, 3268, 3269, 3270, 3271, 3272, 3273, 3274, 3275, 3276, 3277, 3278, 3279, 3280, 3281, 3282, 3283, 3284, 3285, 3286, 3287, 3289, 3292, 3293, 3294, 3300, 3301, 3302, 3303, 3304, 3305, 3309, 3310, 3311, 3312, 3314, 3315, 3317, 3318, 3319, 3321, 3322, 3323, 3324, 3325, 3328, 3329, 3330, 3331, 3332, 3333, 3334, 3363, 3364, 3370, 3371, 3373, 3374, 3375, 3377, 3378, 3379, 3380, 3381, 3384, 3385, 3387, 3388, 3390, 3391, 3392, 3393, 3395, 3396, 3399, 3400, 3401, 3407, 3409, 3412, 3413, 3414, 3415, 3418, 3419, 3420, 3422, 3423, 3424, 3462, 3463, 3464, 3465, 3467, 3468, 3469, 3472, 3475, 3477, 3478, 3480, 3482, 3483, 3485, 3487, 3488, 3489, 3490, 3491, 3494, 3496, 3498, 3500, 3501, 3505, 3506, 3507, 3509, 3512, 3515, 3516, 3517, 3518, 3520, 3521, 3522, 3523, 3525, 3527, 3529, 3530, 3531, 3533, 3537, 3540, 3542, 3544, 3546, 3549, 3557, 3558, 3559, 3561, 3562, 3563, 3564, 3565, 3566, 3567, 3568, 3570, 3571, 3572, 3573, 3575, 3576, 3579, 3580, 3581, 3583, 3584, 3585, 3586, 3588, 3589, 3590, 3591, 3594, 3595, 3596, 3597, 3599, 3607, 3608, 3610, 3612, 3614, 3616, 3617, 3618, 3620, 3621, 3622, 3623, 3624, 3629, 3630, 3631, 3633, 3634, 3635, 3636, 3637, 3638, 3639, 3640, 3641, 3644, 3646, 3647, 3649, 3658, 3659, 3660, 3662, 3663, 3664, 3665, 3666, 3669, 3670, 3672, 3673, 3675, 3677, 3678, 3682, 3683, 3685, 3687, 3688, 3695, 3697, 3698, 3699, 3700, 3701, 3704, 3705, 3707, 3708, 3709, 3711, 3712, 3713, 3714, 3715, 3717, 3718, 3719, 3720, 3722, 3723, 3725, 3726, 3727, 3728, 3730, 3732, 3733, 3735, 3737, 3738, 3739, 3740, 3741, 3744, 3746, 3747, 3749, 3820, 3821, 3822, 3823, 3824, 3825, 3831, 3832, 3833, 3835, 3840, 3842, 3844, 3847, 3850, 3851, 3852, 3854, 3856, 3857, 3858, 3859, 3860, 3862, 3864, 3865, 3869, 3870, 3871, 3873, 3874, 3875, 3878, 3880, 3882, 3885, 3886, 3887, 3888, 3889, 3890, 3891, 3892, 3893, 3895, 3896, 3898, 3900, 3902, 3903, 3904, 3909, 3953, 3954, 3956, 3957, 3958, 3959, 3960, 3962, 3964, 3965, 3966, 3967, 3971, 3229, 3244, 3245, 3246, 3247, 3248, 3252, 3253, 3255, 3256, 3257, 3258, 3259, 3261, 3262, 3263, 3288, 3290, 3291, 3295, 3296, 3297, 3298, 3299, 3306, 3307, 3308, 3313, 3316, 3320, 3326, 3327, 3343, 3344, 3346, 3347, 3348, 3349, 3359, 3362, 3365, 3366, 3367, 3368, 3369, 3372, 3376, 3382, 3383, 3386, 3389, 3394, 3397, 3398, 3402, 3403, 3404, 3405, 3406, 3408, 3410, 3411, 3416, 3417, 3421, 3425, 3426, 3445, 3449, 3452, 3454, 3455, 3456, 3457, 3459, 3466, 3470, 3471, 3473, 3474, 3476, 3479, 3481, 3484, 3486, 3492, 3493, 3495, 3497, 3499, 3502, 3503, 3504, 3508, 3510, 3511, 3513, 3514, 3519, 3524, 3526, 3528, 3532, 3534, 3535, 3536, 3538, 3539, 3541, 3543, 3545, 3547, 3548, 3553, 3554, 3560, 3569, 3574, 3577, 3578, 3582, 3587, 3592, 3593, 3598, 3600, 3601, 3602, 3603, 3604, 3605, 3606, 3609, 3611, 3613, 3615, 3619, 3625, 3626, 3627, 3628, 3632, 3642, 3643, 3645, 3648, 3650, 3651, 3652, 3653, 3654, 3655, 3656, 3657, 3661, 3667, 3668, 3671, 3674, 3676, 3679, 3680, 3681, 3684, 3686, 3692, 3696, 3702, 3703, 3706, 3710, 3716, 3721, 3724, 3729, 3731, 3734, 3736, 3742, 3743, 3745, 3748, 3817, 3819, 3826, 3827, 3828, 3829, 3830, 3834, 3836, 3837, 3838, 3839,
                                                                           3841, 3843, 3845, 3846, 3848, 3849, 3853, 3855, 3861, 3863, 3866, 3867, 3868, 3872, 3876, 3877, 3879, 3881, 3883, 3884, 3894, 3897, 3899, 3901, 3905, 3906, 3907, 3908, 3924, 3947, 3948, 3949, 3952, 3955, 3961, 3963, 3968, 3969, 3970, 3972, 3973, 3974, 3985, 3986, 3989, 3993, 3994, 3997, 3998, 3999), "VIC Country",
                                                  ifelse(df$OWNER_POSTCODE %in% c(3689, 3690, 3218, 3224, 3912, 3936, 3939, 3551, 3139, 3211, 3212, 3213, 3217, 3221, 3222, 3223, 3225, 3226, 3227, 3228, 3230, 3231, 3232, 3335, 3337, 3341, 3342, 3345, 3351, 3352, 3357, 3360, 3361, 3427, 3430, 3431, 3432, 3433, 3434, 3435, 3437, 3438, 3440, 3441, 3442, 3444, 3446, 3447, 3448, 3450, 3451, 3453, 3458, 3460, 3461, 3693, 3750, 3751, 3753, 3754, 3755, 3756, 3757, 3758, 3759, 3760, 3761, 3762, 3763, 3764, 3766, 3767, 3770, 3775, 3777, 3778, 3779, 3781, 3782, 3783, 3785, 3786, 3787, 3788, 3789, 3791, 3792, 3795, 3797, 3799, 3808, 3812, 3813, 3814, 3815, 3816, 3818, 3916, 3918, 3919, 3920, 3921, 3922, 3923, 3925, 3926, 3927, 3928, 3929, 3933, 3937, 3945, 3946, 3950, 3951, 3978, 3979, 3980, 3981, 3984, 3987, 3988, 3990, 3991, 3992, 3995, 3996, 3691, 3233, 3234, 3235, 3236, 3237, 3238, 3239, 3240, 3241, 3242, 3243, 3249, 3250, 3251, 3254, 3260, 3264, 3265, 3266, 3267, 3268, 3269, 3270, 3271, 3272, 3273, 3274, 3275, 3276, 3277, 3278, 3279, 3280, 3281, 3282, 3283, 3284, 3285, 3286, 3287, 3289, 3292, 3293, 3294, 3300, 3301, 3302, 3303, 3304, 3305, 3309, 3310, 3311, 3312, 3314, 3315, 3317, 3318, 3319, 3321, 3322, 3323, 3324, 3325, 3328, 3329, 3330, 3331, 3332, 3333, 3334, 3363, 3364, 3370, 3371, 3373, 3374, 3375, 3377, 3378, 3379, 3380, 3381, 3384, 3385, 3387, 3388, 3390, 3391, 3392, 3393, 3395, 3396, 3399, 3400, 3401, 3407, 3409, 3412, 3413, 3414, 3415, 3418, 3419, 3420, 3422, 3423, 3424, 3462, 3463, 3464, 3465, 3467, 3468, 3469, 3472, 3475, 3477, 3478, 3480, 3482, 3483, 3485, 3487, 3488, 3489, 3490, 3491, 3494, 3496, 3498, 3500, 3501, 3505, 3506, 3507, 3509, 3512, 3515, 3516, 3517, 3518, 3520, 3521, 3522, 3523, 3525, 3527, 3529, 3530, 3531, 3533, 3537, 3540, 3542, 3544, 3546, 3549, 3557, 3558, 3559, 3561, 3562, 3563, 3564, 3565, 3566, 3567, 3568, 3570, 3571, 3572, 3573, 3575, 3576, 3579, 3580, 3581, 3583, 3584, 3585, 3586, 3588, 3589, 3590, 3591, 3594, 3595, 3596, 3597, 3599, 3607, 3608, 3610, 3612, 3614, 3616, 3617, 3618, 3620, 3621, 3622, 3623, 3624, 3629, 3630, 3631, 3633, 3634, 3635, 3636, 3637, 3638, 3639, 3640, 3641, 3644, 3646, 3647, 3649, 3658, 3659, 3660, 3662, 3663, 3664, 3665, 3666, 3669, 3670, 3672, 3673, 3675, 3677, 3678, 3682, 3683, 3685, 3687, 3688, 3695, 3697, 3698, 3699, 3700, 3701, 3704, 3705, 3707, 3708, 3709, 3711, 3712, 3713, 3714, 3715, 3717, 3718, 3719, 3720, 3722, 3723, 3725, 3726, 3727, 3728, 3730, 3732, 3733, 3735, 3737, 3738, 3739, 3740, 3741, 3744, 3746, 3747, 3749, 3820, 3821, 3822, 3823, 3824, 3825, 3831, 3832, 3833, 3835, 3840, 3842, 3844, 3847, 3850, 3851, 3852, 3854, 3856, 3857, 3858, 3859, 3860, 3862, 3864, 3865, 3869, 3870, 3871, 3873, 3874, 3875, 3878, 3880, 3882, 3885, 3886, 3887, 3888, 3889, 3890, 3891, 3892, 3893, 3895, 3896, 3898, 3900, 3902, 3903, 3904, 3909, 3953, 3954, 3956, 3957, 3958, 3959, 3960, 3962, 3964, 3965, 3966, 3967, 3971, 3229, 3244, 3245, 3246, 3247, 3248, 3252, 3253, 3255, 3256, 3257, 3258, 3259, 3261, 3262, 3263, 3288, 3290, 3291, 3295, 3296, 3297, 3298, 3299, 3306, 3307, 3308, 3313, 3316, 3320, 3326, 3327, 3343, 3344, 3346, 3347, 3348, 3349, 3359, 3362, 3365, 3366, 3367, 3368, 3369, 3372, 3376, 3382, 3383, 3386, 3389, 3394, 3397, 3398, 3402, 3403, 3404, 3405, 3406, 3408, 3410, 3411, 3416, 3417, 3421, 3425, 3426, 3445, 3449, 3452, 3454, 3455, 3456, 3457, 3459, 3466, 3470, 3471, 3473, 3474, 3476, 3479, 3481, 3484, 3486, 3492, 3493, 3495, 3497, 3499, 3502, 3503, 3504, 3508, 3510, 3511, 3513, 3514, 3519, 3524, 3526, 3528, 3532, 3534, 3535, 3536, 3538, 3539, 3541, 3543, 3545, 3547, 3548, 3553, 3554, 3560, 3569, 3574, 3577, 3578, 3582, 3587, 3592, 3593, 3598, 3600, 3601, 3602, 3603, 3604, 3605, 3606, 3609, 3611, 3613, 3615, 3619, 3625, 3626, 3627, 3628, 3632, 3642, 3643, 3645, 3648, 3650, 3651, 3652, 3653, 3654, 3655, 3656, 3657, 3661, 3667, 3668, 3671, 3674, 3676, 3679, 3680, 3681, 3684, 3686, 3692, 3696, 3702, 3703, 3706, 3710, 3716, 3721, 3724, 3729, 3731, 3734, 3736, 3742, 3743, 3745, 3748, 3817, 3819, 3826, 3827, 3828, 3829, 3830, 3834, 3836, 3837, 3838,
                                                                                  3839, 3841, 3843, 3845, 3846, 3848, 3849, 3853, 3855, 3861, 3863, 3866, 3867, 3868, 3872, 3876, 3877, 3879, 3881, 3883, 3884, 3894, 3897, 3899, 3901, 3905, 3906, 3907, 3908, 3924, 3947, 3948, 3949, 3952, 3955, 3961, 3963, 3968, 3969, 3970, 3972, 3973, 3974, 3985, 3986, 3989, 3993, 3994, 3997, 3998, 3999), "Wodonga", "Other"))))))
  
  df$REGION <- as.factor(df$REGION)
  
  # Getting rid of the useless ID variables
  df <- select(df, -DRIVER_ID, - VEHICLE_ID, -ACCIDENT_NO, -OWNER_POSTCODE)
  
  
  # Adding factors to the time of the incident (time of day)
  df$ACCIDENTTIME <- strptime(df$ACCIDENTTIME, format = "%H:%M:%S")
  times <- c("dawn", "early morning", "midday", "afternoon", "evening"
             , "nighttime")
  df$TOD <- cut(df$ACCIDENTTIME$hour, breaks = c(0, 4, 8, 12, 16, 20, 24),
                labels = times, include.lowest = TRUE)
  df$hour <- hour(df$ACCIDENTTIME)
  df <- select(df, -ACCIDENTTIME)
  
  # Factoring dates by month
  df$ACCIDENTDATE <- as.Date(df$ACCIDENTDATE, format = "%d/%m/%Y")
  df$Month <- format(df$ACCIDENTDATE, "%B")
  df$Month <- as.factor(df$Month)
  month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
  df$Month <- factor(df$Month, month_order)
  
  df <- df %>% mutate(YEAR_OF_ACCIDENT = year(ACCIDENTDATE))
  df <- df %>% mutate(VEHICLE_AGE = YEAR_OF_ACCIDENT - VEHICLE_YEAR_MANUF)
  
  # Getting rid of the 'U' gender (ambivalent to objective)  
  df <- df[df$SEX != "U",]
  df$SEX <- droplevels(df$SEX, exclude = "U")
  
  # Factoring age of vehicle
  decades <- c("80s", "90s", "00s", "10s")
  df$VEHICLE_DECADE <- cut(df$VEHICLE_YEAR_MANUF, 
                           breaks = c(1980, 1990, 2000, 2010, 2020),
                           labels = decades, include.lowest = TRUE)
  
  
  # Making the day of the week be factored in correct order
  day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  df$DAY_OF_WEEK <- factor(df$DAY_OF_WEEK, day_order)
  
  df <- df %>% mutate(WEEKEND = ifelse(df$DAY_OF_WEEK == "Saturday" | df$DAY_OF_WEEK == "Sunday", TRUE, FALSE))
  
  
  return(df)
}
df <- add_variables(df)



# DATA ANALYSIS



# Creates pie charts of factor all variables and displays on a single chart.
create_combined_pie_chart <- function(df) {
  # Filter factor variables
  factor_vars <- df %>%
    select_if(is.factor)
  
  # Create pie charts for each factor variable
  pie_charts <- lapply(names(factor_vars), function(var) {
    ggplot(data = df, aes(x = 1, fill = !!sym(var))) +
      geom_bar(width = 1) +
      coord_polar(theta = "y") +
      labs(title = var, fill = "") +
      theme_void()
  })
  
  # Combine pie charts into one chart
  combined_chart <- wrap_plots(pie_charts, ncol = 5)
  
  # Return the combined chart
  return(combined_chart)
}
create_combined_pie_chart(df)
create_combined_pie_chart(df[df$fatal == TRUE,])




#Creates a fequency line graph between two discrete variables
make_frequency_graphs <- function(df, var1, var2) {
  df_freq <- df %>%
    group_by({{ var1 }}, {{ var2 }}) %>%
    summarise(frequency = n())
  
  ggplot(data = df_freq, aes(x = {{ var1 }}, y = frequency, group = {{ var2 }})) +
    geom_line(aes(color = {{ var2 }})) +
    geom_point(aes(color = {{ var2 }})) + theme(axis.text.x=element_text(size=15), legend.text = element_text(size = 10), axis.text.y=element_text(size=15))
}
make_frequency_graphs(df, hour, REGION)
make_frequency_graphs(df, hour, SURFACE_COND)
make_frequency_graphs(df, SPEED_ZONE, Age.Group)
make_frequency_graphs(df, hour, SEX)
make_frequency_graphs(df, hour, SPEED_ZONE)
make_frequency_graphs(df, SPEED_ZONE, HELMET_BELT_WORN)
make_frequency_graphs(df, VEHICLE_AGE, Age.Group)
make_frequency_graphs(df, AGE, WEEKEND)
make_frequency_graphs(df, SPEED_ZONE, VEHICLE_TYPE)
make_frequency_graphs(df, hour, VEHICLE_TYPE)
make_frequency_graphs(df, DAY_OF_WEEK, VEHICLE_TYPE)
make_frequency_graphs(df, SPEED_ZONE, ROAD_GEOMETRY)
make_frequency_graphs(df[df$ROAD_GEOMETRY != "Other" & df$ACCIDENT_TYPE != "Struck animal" & df$ACCIDENT_TYPE != "No collision and no object struck" & df$ACCIDENT_TYPE != "Fall from or in moving vehicle",], ACCIDENT_TYPE, ROAD_GEOMETRY)
make_frequency_graphs(df[df$ROAD_GEOMETRY != "Other" & df$ACCIDENT_TYPE != "Struck animal" & df$ACCIDENT_TYPE != "No collision and no object struck" & df$ACCIDENT_TYPE != "Fall from or in moving vehicle",], SPEED_ZONE, ACCIDENT_TYPE)


make_frequency_graphs(df[df$fatal == TRUE,], hour, SURFACE_COND)
make_frequency_graphs(df[df$fatal == TRUE,], SPEED_ZONE, Age.Group)
make_frequency_graphs(df[df$fatal == TRUE,], hour, SEX)
make_frequency_graphs(df[df$fatal == TRUE,], hour, SPEED_ZONE)
make_frequency_graphs(df[df$fatal == TRUE,], Age.Group, HELMET_BELT_WORN)
make_frequency_graphs(df[df$fatal == TRUE,], SPEED_ZONE, HELMET_BELT_WORN)
mosaic_df <- df

# Mosaic plot 
levels(mosaic_df$ACCIDENT_TYPE) <- c("Fixed Object", "Other object", "Vehicle", "Fall onto", "None", "Animal", "Pedestrian", "Overturned")
mosaic_df <-mosaic_df[df$ROAD_GEOMETRY != "Other" & df$ACCIDENT_TYPE != "Animal" & df$ACCIDENT_TYPE != "None" & df$ACCIDENT_TYPE != "Fall onto",]
mosaic_df$ACCIDENT_TYPE <- droplevels(mosaic_df$ACCIDENT_TYPE, exclude = c("None", "Fall onto", "Other object"))
mosaic_df$ROAD_GEOMETRY <- droplevels(mosaic_df$ROAD_GEOMETRY, exclude = c("Other"))
levels(df$ACCIDENT_TYPE) <- c("Fixed Object", "Other object", "Vehicle", "Fall onto", "None", "Animal", "Pedestrian", "Overturned")
mosaicplot(table(mosaic_df$ROAD_GEOMETRY, mosaic_df$ACCIDENT_TYPE, mosaic_df$SPEED_ZONE),
           main = "Mosaic Plot between Road Geometry, Accident Type and Speed Zone",
           color = TRUE,
           las = 1,
           cex.axis = 0.8)
mosaicplot(table(mosaic_df$REGION, mosaic_df$fatal, mosaic_df$SPEED_ZONE),
           main = "Mosaic Plot",
           color = TRUE,
           las = 1)



# Creates a line graph depicting mean fatality over multiple levels of factors
mean_fatality_line_graph <- function(df, var) {
  df_mean_fat <- df %>%
    group_by({{ var }}) %>%
    summarise(likelihood = mean(fatal))
  
  # Create the line graph
  ggplot(data = df_mean_fat, aes(x = {{ var }}, y = likelihood, group = 1)) +
    geom_line(color = "blue", size = 1.5) +    
    geom_point(color = "red", size = 3) +  
    theme_minimal() +                            
    theme(plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(face = "bold", size = 12), 
          axis.text = element_text(size = 10),                 
          legend.position = "none")                             
}
mean_fatality_line_graph(df, hour)
mean_fatality_line_graph(df, SURFACE_COND)
mean_fatality_line_graph(df, DAY_OF_WEEK)
mean_fatality_line_graph(df, VEHICLE_AGE)
mean_fatality_line_graph(df, REGION)
mean_fatality_line_graph(df, VEHICLE_BODY_STYLE)
mean_fatality_line_graph(df, VEHICLE_MAKE)
mean_fatality_line_graph(df, VEHICLE_TYPE)
mean_fatality_line_graph(df, FUEL_TYPE)
mean_fatality_line_graph(df, VEHICLE_COLOUR)
mean_fatality_line_graph(df, OWNER_POSTCODE)


boxplot(df$SPEED_ZONE ~ df$fatal)
country <- df[df$REGION == "VIC Country",]
boxplot(country$SPEED_ZONE ~ country$fatal, xlab = "Country Victoria?", ylab = "Speed Zone", main = "")

fatal_df <- df[df$fatal == TRUE,]
fatal_df$COUNTRY <- ifelse(fatal_df$REGION == "VIC Country", "Yes", "No")
boxplot(fatal_df$SPEED_ZONE ~ fatal_df$COUNTRY)


# Creates a heatmap of mean fatality between two variables
create_fatal_heat_map <- function(df, var1, var2) {
  df_mean_fat <- df %>%
    group_by({{ var1 }}, {{ var2 }}) %>%
    summarise(likelihood = mean(fatal))
  
  ggplot(df_mean_fat, aes(x = {{ var1 }}, y = {{ var2 }}, z = likelihood, fill = likelihood)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(z = "Likelihood") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

create_fatal_heat_map(df, Age.Group, SPEED_ZONE)
create_fatal_heat_map(df, hour, SEX)
create_fatal_heat_map(df, ACCIDENT_TYPE, SPEED_ZONE)
create_fatal_heat_map(df, SPEED_ZONE, HELMET_BELT_WORN)
create_fatal_heat_map(df, WEEKEND, Age.Group)
create_fatal_heat_map(df, Age.Group, SPEED_ZONE)
create_fatal_heat_map(df, VEHICLE_TYPE, SPEED_ZONE)
create_fatal_heat_map(df, DAY_OF_WEEK, hour)
create_fatal_heat_map(df, VEHICLE_AGE, hour)
create_fatal_heat_map(df, hour, WEEKEND)
create_fatal_heat_map(df, SPEED_ZONE, ROAD_GEOMETRY)
create_fatal_heat_map(df[df$ROAD_GEOMETRY != "Other" & df$ACCIDENT_TYPE != "Animal" & df$ACCIDENT_TYPE != "None" & df$ACCIDENT_TYPE != "Fall onto",], ACCIDENT_TYPE, ROAD_GEOMETRY)

#Creates a stacked bar graph between two variables
stacked_bar <- function(df, group, category) {
  ggplot(data = df, aes(x = {{ group }}, fill = {{ category }})) +
    geom_bar(stat = "count", position = "fill") +
    scale_y_continuous(labels = scales::percent) + theme(axis.text.x=element_text(size=15), legend.text = element_text(size = 10), axis.text.y=element_text(size=15))
}
summary(df$Month)
levels(df$Month) <- c("J", "F", "M", "A", "My", "Jn", "Jl", "Au", "S", "O", "N", "D")
stacked_bar(df, LIGHT_CONDITION, SEX)
stacked_bar(df, Month, SURFACE_COND)
nrow(df[df$SURFACE_COND == "Wet",])/nrow(df)
stacked_bar(df, Month, LIGHT_CONDITION)
nrow(df[df$LIGHT_CONDITION == "Dark Street lights on",])/nrow(df)

stacked_bar(df, Age.Group, WEEKEND)
stacked_bar(df, Age.Group, SPEED_ZONE)
stacked_bar(df, hour, WEEKEND)
stacked_bar(df, SPEED_ZONE, REGION)




# Creates staked bar graph for all variables against a pivot variables
stacked_bars <- function(df, category) {
  plot_list <- list()  
  
  # Loop through each factor variable
  for (var in names(df)) {
    plot <- ggplot(data = df, aes_string(x = var, fill = category)) +
      geom_bar(stat = "count", position = "fill") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = var, y = "Percentage")
    
    plot_list[[var]] <- plot  # Store each plot in the list
  }
  
  # Combine all plots on one chart
  combined_plot <- do.call(gridExtra::grid.arrange, plot_list)
  return(combined_plot)
}
stacked_bars(df, "SEX")
stacked_bars(df, "Age.Group")
df$SPEED_ZONE <- as.factor(df$SPEED_ZONE)
stacked_bars(df, "SPEED_ZONE")
stacked_bars(df, "SURFACE_COND")
stacked_bars(df, "LIGHT_CONDITION")
stacked_bars(df, "WEEKEND")
stacked_bars(df, "VEHICLE_BODY")
stacked_bars(df, "ACCIDENT_TYPE")

stacked_bars(df, "SEX")
stacked_bars(df, "Age.Group")
df$SPEED_ZONE <- as.factor(df$SPEED_ZONE)
stacked_bars(df[df$fatal == TRUE,], "SPEED_ZONE")
stacked_bars(df, "SURFACE_COND")
stacked_bars(df[df$fatal == TRUE,], "LIGHT_CONDITION")
stacked_bars(df[df$fatal == TRUE,], "WEEKEND")
stacked_bars(df, "VEHICLE_BODY")
stacked_bars(df, "REGION")
stacked_bars(df[df$fatal == TRUE,], "REGION")






######### PART 2 ##########



model_df <- select(df, -c(AGE, VEHICLE_DECADE, DAY_OF_WEEK, hour, VEHICLE_YEAR_MANUF, ACCIDENTDATE)) 
#summary(df)
#model_df <- select(df, c(SEX, Age.Group, HELMET_BELT_WORN, VEHICLE_YEAR_MANUF, VEHICLE_TYPE, TOTAL_NO_OCCUPANTS, ACCIDENT_TYPE, LIGHT_CONDITION, ROAD_GEOMETRY, SPEED_ZONE, SURFACE_COND, ATMOSPH_COND, REGION, TOD, WEEKEND, fatal))


# Reformatting data for model friendly 
x_var <- model.matrix(fatal ~. , model_df)[,-1]
x_var <- as.data.frame(x_var)
y_var <- model_df$fatal
set.seed(50)

index <- createDataPartition(model_df$fatal, p = 0.8, list = FALSE)
train_df <- model_df[index,]
test_df <- model_df[-index,]
test_original <- model_df


##### IMBALANCE TECHNIQUES ######

# ONLY CHOOSE ONE AT A TIME OR NONE FOR COST SENSITIVE

# UPSAMPLING
split2 <- createDataPartition(train_df$fatal, p = 0.5, list = FALSE)
new_train <- train_df[split2,]
old_train <- train_df[split2,]
newdata1 = upSample(new_train,as.factor(new_train$fatal))
newdata1$Class = NULL
train_df = rbind(newdata1,old_train)
table(train_df$fatal)



#ADASYN
index <- createDataPartition(model_df$fatal, p = 0.8, list = FALSE)
x_var <- as.data.frame(x_var)
x_train <- x_var[index,]
y_train <- y_var[index]
x_test <- x_var[-index,]
y_test <- y_var[-index]

index2 <- createDataPartition(y_train, p = 0.5, list = FALSE)
x_new <- x_train[index2,]
y_new <- y_train[index2]
x_old <- x_train[-index2,]
y_old <- y_train[-index2]
train_old <- as.data.frame(cbind(x_old,y_old))
names(train_old)[ncol(train_old)] <- "fatal"

adasyn <- ADAS(x_new,y_new,K=5)
#adasyn <- ADAS(x_train,y_train,K=5)

new <- adasyn$data
new$class <- ifelse(new$class == "TRUE", TRUE, FALSE)
new <- as.data.frame(new)
names(new)[ncol(new)] <- "fatal"
new <- rbind(new, train_old)
train_df <- new
test_df <- as.data.frame(cbind(x_test,y_test))
names(test_df)[ncol(test_df)] <- "fatal"

###### STEPWISE SELECTION #########

weight <- 5
weights <- ifelse(train_df$fatal == TRUE, weight, 1)

#regfit.backward <- regsubsets(fatal ~., train_df, method = "backward", nvmax= ncol(x_var))
regfit.backward <- regsubsets(fatal ~., train_df, method = "backward", nvmax= ncol(x_var), weights = weights)

#regfit.forward <- regsubsets(fatal ~., train_df, method = "forward", nvmax= ncol(x_var))
regfit.forward <- regsubsets(fatal ~., train_df, method = "forward", nvmax= ncol(x_var), weights = weights)

backward.summary <- summary(regfit.backward)

forward.summary <- summary(regfit.forward)






######### STEPWISE WITH CV ############
predict.regsubsets <- function (object , newdata , id) {
  mat <- model.matrix(fatal ~., newdata)
  coefi <- coef (object , id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
} 


k <- 20
folds <- createFolds(factor(model_df$fatal), k = k, list = FALSE) # this stratifies automatically
cv.errors <- matrix (NA, k, ncol(x_var), dimnames = list(NULL , paste (1:(ncol(x_var)))))
p_area <- matrix (NA, k, ncol(x_var), dimnames = list(NULL , paste (1:(ncol(x_var)))))
for (j in 1:k) {
  best.fit <- regsubsets(fatal ~. ,
                         data = model_df[folds != j, ],
                         nvmax = ncol(x_var), method = "forward")
  for (i in 1:(ncol(x_var))) {
    pred <- predict(best.fit , model_df[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean ((model_df$fatal[folds == j] - pred)^2)
    p_area[j, i] <-
      pr.curve(scores.class0 = pred, weights.class0 = model_df$fatal[folds == j], curve = TRUE)$auc.integral
  }
}
mean.cv.errors.forwards <- apply (cv.errors , 2, mean)
mean.p.area.forwards <- apply (p_area , 2, mean)
which.min(mean.cv.errors.forwards)
which.max(mean.p.area.forwards)



cv.errors <- matrix (NA, k, ncol(x_var), dimnames = list(NULL , paste (1:(ncol(x_var)))))
p_area <- matrix (NA, k, ncol(x_var), dimnames = list(NULL , paste (1:(ncol(x_var)))))
for (j in 1:k) {
  best.fit <- regsubsets(fatal ~. ,
                         data = model_df[folds != j, ],
                         nvmax = ncol(x_var), method = "backward")
  for (i in 1:(ncol(x_var))) {
    pred <- predict(best.fit , model_df[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean ((model_df$fatal[folds == j] - pred)^2)
    p_area[j, i] <-
      pr.curve(scores.class0 = pred, weights.class0 = model_df$fatal[folds == j], curve = TRUE)$auc.integral
  }
}
mean.cv.errors.backward <- apply (cv.errors , 2, mean)
mean.p.area.backwards <- apply (p_area , 2, mean)
which.min(mean.cv.errors.backward)
which.max(mean.p.area.backwards)

cv.forward.model <- regsubsets(fatal ~ ., data = model_df, nbest = 1, nvmax = ncol(x_var), method = "forward", really.big = TRUE)
cv.backward.model <- regsubsets(fatal ~ ., data = model_df, nbest = 1, nvmax = ncol(x_var), method = "backward", really.big = TRUE)





########### CHECKING ##########


# These act as switches by replacing the train and test dataframes with only
# the necessary variables depending on which model is to be tested.

# names.backward.cp <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1] # }} 15.8, 10.7
# names.backward.bic <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1] # }} 16.24, 10.89
# names.forward.cp <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1] # }} 15.55, 10.71
# names.forward.bic <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1] #  }} 15.57, 10.56
# names.backward.cv <- names(coef(cv.backward.model,which.min(mean.cv.errors.backward)))[-1]  # }} 9.07/15.04, 9.84
# names.forward.cv <- names(coef(cv.forward.model,which.min(mean.cv.errors.forwards)))[-1]  # }} 9.07/15.04, 9.98
# names.forward.p <- names(coef(cv.forward.model,which.max(mean.p.area.forwards)))[-1] # 9.07
# names.backward.p <- names(coef(cv.forward.model,which.max(mean.p.area.backwards)))[-1]

# names.backward.cp.5 <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1] # 16.7, 10.3
names.backward.bic.5 <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1] # 16.84, 10.32
# names.forward.cp.5 <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1] #16.79, 10.314
# names.forward.bic.5 <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1] # 16.83, 10.31
 
# names.backward.cp.10 <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1] #16.59, 10.2
# names.backward.bic.10 <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1] #16.56, 10.21
# names.forward.cp.10 <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1]
# names.forward.bic.10 <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1]

# a.names.backward.cp <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1] #11, 16.11
# a.names.backward.bic <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1] #10.85, 16.96
# a.names.forward.cp <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1]
# a.names.forward.bic <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1]
# a.names.backward.cv <- names(coef(cv.backward.model,which.min(mean.cv.errors.backward)))[-1]
# a.names.forward.cv <- names(coef(cv.forward.model,which.min(mean.cv.errors.forwards)))[-1]
# a.names.forward.p <- names(coef(cv.forward.model,which.max(mean.p.area.forwards)))[-1]
# a.names.backward.p <- names(coef(cv.forward.model,which.max(mean.p.area.backwards)))[-1]

# u.names.backward.cp <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1]
# u.names.backward.bic <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1]
# u.names.forward.cp <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1]
# u.names.forward.bic <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1]
# u.names.backward.cv <- names(coef(cv.backward.model,which.min(mean.cv.errors.backward)))[-1]
# u.names.forward.cv <- names(coef(cv.forward.model,which.min(mean.cv.errors.forwards)))[-1]
# u.names.forward.p <- names(coef(cv.forward.model,which.max(mean.p.area.forwards)))[-1]
# u.names.backward.p <- names(coef(cv.forward.model,which.max(mean.p.area.backwards)))[-1]


# Create data frames for testing

names <- names.backward.bic.5

train_model_names <- as.data.frame(model.matrix(fatal ~., train_df))[,-1]
train_model <- train_model_names[, names]
train_model <- cbind(train_model, train_df$fatal)
names(train_model)[ncol(train_model)] <- "fatal"

test_model <- as.data.frame(model.matrix(fatal ~., test_df))[,-1]
test_model <- test_model[, names]
test_model <- cbind(test_model, test_df$fatal)
names(test_model)[ncol(test_model)] <- "fatal"

test_model_original <- as.data.frame(model.matrix(fatal ~., model_df))[,-1]
test_model_original <- cbind(test_model_original, model_df$fatal)
names(test_model_original) <- append(names(train_model_names), "fatal")
test_model_original <- test_model_original[, append(names, "fatal")]

#Normal logistic regression
par(mfrow = c(2,2))
weight <- 5
weights <- ifelse(train_df$fatal == TRUE, weight, 1)
model <- glm(fatal ~. , data = train_model, family = "binomial", weights = weights)

#summary(model)

#train_model <- significance(model, train_model, 0.01)

#pred_train <- predict(model, newdata = train_model, type = "response")
#confusionMatrix(data = as.factor(ifelse(as.numeric(pred_train > 0.3),TRUE,FALSE)), reference = as.factor(train_model$fatal))

pred_test <- predict(model, newdata = test_model, type = "response")
#confusionMatrix(data = as.factor(ifelse(as.numeric(pred_test > 0.57),TRUE,FALSE)), reference = as.factor(test_df$fatal))

pred_test_original <- predict(model, newdata = test_model_original, type = "response")
#confusionMatrix(data = as.factor(ifelse(as.numeric(pred_test_original > 0.5),TRUE,FALSE)), reference = as.factor(test_model_original$fatal))

pr_curve <- pr.curve(scores.class0 = pred_test, weights.class0 = test_df$fatal, curve = TRUE)
plot(pr_curve)


thresholds <- seq(0, 1, by = 0.01)
f1_scores <- vector("numeric", length = length(thresholds))
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  predicted_labels <- ifelse(pred_test >= threshold, 1, 0)
  
  # Calculate precision, recall, and F1 score
  precision <- sum(predicted_labels == 1 & test_df$fatal == 1) / sum(predicted_labels == 1)
  recall <- sum(predicted_labels == 1 & test_df$fatal == 1) / sum(test_df$fatal == 1)
  f1_scores[i] <- 2 * (precision * recall) / (precision + recall)
}
max(f1_scores, na.rm = TRUE)
which.max(f1_scores)
thresholds[which.max(f1_scores)]


roc_curve <- roc(test_model_original$fatal, pred_test_original)
pr_curve <- pr.curve(scores.class0 = pred_test_original, weights.class0 = test_model_original$fatal, curve = TRUE)
auc(roc_curve)
plot(roc_curve)
plot(pr_curve)

thresholds <- seq(0.1, 0.9, by = 0.02)
f1_scores <- vector("numeric", length = length(thresholds))
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  predicted_labels <- ifelse(pred_test_original >= threshold, 1, 0)
  
  # Calculate precision, recall, and F1 score
  precision <- sum(predicted_labels == 1 & test_model_original$fatal == 1) / sum(predicted_labels == 1)
  recall <- sum(predicted_labels == 1 &test_model_original$fatal == 1) / sum(test_model_original$fatal == 1)
  f1_scores[i] <- 2 * (precision * recall) / (precision + recall)
}
max(f1_scores, na.rm = TRUE)
which.max(f1_scores)

summary(model)


################ RIDGE and LASSO regression ####################
x_train <- as.matrix(train_model[,-ncol(train_model)])
y_train <- train_model[,ncol(train_model)]

x_test <- as.matrix(test_model[,-ncol(test_model)])
y_test <-test_model[,ncol(test_model)]

x_total <- rbind(x_train, x_test)
y_total <- c(y_train, y_test)

weights <- ifelse(y_total == TRUE, weight, 1)
k <- 20
set.seed(51)
folds <- createFolds(factor(y_total), k = k, list = FALSE) # this stratifies automatically


grid <- exp(1)^ seq (-3, -9, length = 100)
lasso.mod <- glmnet(x_total, y_total, alpha = 1,
                    lambda = grid, thresh = 1e-12, standardize = TRUE, weights = weights)
plot(lasso.mod, xvar = "lambda", label = TRUE)

set.seed(51)
lasso_model1 <- cv.glmnet(y = y_total, x = x_total , family = "binomial", foldid = folds, 
                          type.measure = "class", alpha = 1, weights = weights)
plot (lasso_model1)

lasso_best_lambda <- lasso_model1$lambda.min
log(lasso_best_lambda)
lasso.pred.best <- predict(lasso_model1 , s = lasso_best_lambda ,
                           newx = x_total, type = "response")
lass.coef.best <- predict(lasso_model1 , s = lasso_best_lambda ,
                          newx = x_total, type = "coefficients")
pr_curve <- pr.curve(scores.class0 = lasso.pred.best, weights.class0 = y_test, curve = TRUE)
plot(pr_curve) # 10.37
confusionMatrix(data = as.factor(ifelse(as.numeric(lasso.pred.best > 0.3),TRUE,FALSE)), reference = as.factor(y_test))

lasso.one.se <- lasso_model1$lambda.1se
log(lasso.one.se)
lasso.one.se.pred <- predict(lasso_model1 , s = lasso.one.se ,
                             newx = x_test, type = "response")
lass.coef.se <- predict(lasso_model1 , s = lasso.one.se ,
                        newx = x_total, type = "coefficients")
roc_curve <- roc(y_test, lasso.one.se.pred)
pr_curve <- pr.curve(scores.class0 = lasso.one.se.pred, weights.class0 = y_test, curve = TRUE)
plot(pr_curve) 
confusionMatrix(data = as.factor(ifelse(as.numeric(lasso.one.se.pred > 0.5),TRUE,FALSE)), reference = as.factor(y_test))

thresholds <- seq(0.1, 0.9, by = 0.02)
f1_scores <- vector("numeric", length = length(thresholds))
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  predicted_labels <- ifelse(lasso.one.se.pred >= threshold, 1, 0)
  
  precision <- sum(predicted_labels == 1 & y_test == 1) / sum(predicted_labels == 1)
  recall <- sum(predicted_labels == 1 & y_test == 1) / sum(y_test == 1)
  f1_scores[i] <- 2 * (precision * recall) / (precision + recall)
}
max(f1_scores, na.rm = TRUE)
which.max(f1_scores)
thresholds[which.max(f1_scores)]

summary(df)
summary(x_var)

lasso_model2 <- cv.glmnet(y = y_total, x = x_total , family = "binomial", foldid = folds, 
                          type.measure = "class", alpha = 1, weights = weights)
plot (lasso_model2)

lasso_best_lambda <- lasso_model2$lambda.min
log(lasso_best_lambda)
lasso.pred.best <- predict(lasso_model2 , s = lasso_best_lambda ,
                           newx = x_test, type = "response")
lass.coef.best <- predict(lasso_model2 , s = lasso_best_lambda ,
                          newx = x_test, type = "coefficients")
pr_curve <- pr.curve(scores.class0 = lasso.pred.best, weights.class0 = y_test, curve = TRUE)
plot(pr_curve) # 10.37
confusionMatrix(data = as.factor(ifelse(as.numeric(lasso.pred.best > 0.5),TRUE,FALSE)), reference = as.factor(y_test))

lasso.one.se <- lasso_model2$lambda.1se
log(lasso.one.se)
lasso.one.se.pred <- predict(lasso_model2 , s = lasso.one.se ,
                             newx = x_test, type = "response")
lass.coef.se <- predict(lasso_model2 , s = lasso.one.se ,
                        newx = x_test, type = "coefficients")
roc_curve <- roc(y_test, lasso.one.se.pred)
pr_curve <- pr.curve(scores.class0 = lasso.one.se.pred, weights.class0 = y_test, curve = TRUE)
plot(pr_curve) # 10.31
confusionMatrix(data = as.factor(ifelse(as.numeric(lasso.one.se.pred > 0.5),TRUE,FALSE)), reference = as.factor(y_test))

thresholds <- seq(0.1, 0.9, by = 0.02)
f1_scores <- vector("numeric", length = length(thresholds))
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  predicted_labels <- ifelse(lasso.one.se.pred >= threshold, 1, 0)
  
  # Calculate precision, recall, and F1 score
  precision <- sum(predicted_labels == 1 & y_test == 1) / sum(predicted_labels == 1)
  recall <- sum(predicted_labels == 1 & y_test == 1) / sum(y_test == 1)
  f1_scores[i] <- 2 * (precision * recall) / (precision + recall)
}
max(f1_scores, na.rm = TRUE)
which.max(f1_scores)
thresholds[which.max(f1_scores)]





################ PART 3 ################
library("caTools")

#summary(df)

prediction_df <- select(df, c(SEX, Age.Group, LICENCE_STATE, HELMET_BELT_WORN, VEHICLE_MAKE, VEHICLE_TYPE, FUEL_TYPE, VEHICLE_YEAR_MANUF, REGION, TOTAL_NO_OCCUPANTS, VEHICLE_COLOUR, VEHICLE_BODY_STYLE, fatal))


all_x <- as.data.frame(model.matrix(fatal~., data = prediction_df)[,-1])

set.seed(1)
index <- createDataPartition(prediction_df$fatal, p = 0.8, list = FALSE)
train_df <- prediction_df[index,]
x_train <- train_df[,-ncol(train_df)]
y_train <- train_df[,ncol(train_df)]

nottrain_df <- prediction_df[-index,]
index2 <- createDataPartition(nottrain_df$fatal, p = 0.5, list = FALSE)
valid_df <- nottrain_df[index2,]
x_valid <- valid_df[,-ncol(valid_df)]
y_valid <- valid_df[,ncol(valid_df)]

test_df <- nottrain_df[-index2,]
non_test_df <- rbind(train_df, valid_df)

######### REBALANCING TECHNIQUES ###############

#UPSAMPLING
# split2 <- createDataPartition(train_df$fatal, p = 0.50, list = FALSE)
# new_train <- train_df[split2,]
# old_train <- train_df[-split2,]
# newdata1 = upSample(new_train,as.factor(new_train$fatal))
# newdata1$Class = NULL
# train_df = rbind(newdata1,old_train)

train_df <- upSample(train_df, as.factor(train_df$fatal))
train_df$Class = NULL

# #ADASYN
# x_train <- as.data.frame(model.matrix(~., x_train))[-1]
# x_valid <- as.data.frame(model.matrix(~., x_valid))[-1]
# index2 <- createDataPartition(y_train, p = 0.5, list = FALSE)
# x_new <- x_train[index2,]
# y_new <- y_train[index2]
# x_old <- x_train[-index2,]
# y_old <- y_train[-index2]
# train_old <- as.data.frame(cbind(x_old,y_old))
# names(train_old)[ncol(train_old)] <- "fatal"
# 
# adasyn <- ADAS(x_new,y_new,K=5)
# #adasyn <- ADAS(x_train,y_train,K=5)
# 
# new <- adasyn$data
# new$class <- ifelse(new$class == "TRUE", TRUE, FALSE)
# new <- as.data.frame(new)
# names(new)[ncol(new)] <- "fatal"
# new <- rbind(new, train_old)
# train_df <- new
# valid_df <- cbind(x_valid, y_valid)
# names(valid_df)[ncol(valid_df)] <- "fatal"
# table(train_df$fatal)



############ Logistic Regression ####################

########## STEWPISE ############
weight <- 1
weights <- ifelse(train_df$fatal == TRUE, weight, 1)

#regfit.backward <- regsubsets(fatal ~., train_df, method = "backward", nvmax= ncol(x_var))
regfit.backward <- regsubsets(fatal ~., train_df, method = "backward", nvmax= ncol(all_x), weights = weights)

#regfit.forward <- regsubsets(fatal ~., train_df, method = "forward", nvmax= ncol(x_var))
regfit.forward <- regsubsets(fatal ~., train_df, method = "forward", nvmax= ncol(all_x), weights = weights)

backward.summary <- summary(regfit.backward)

forward.summary <- summary(regfit.forward)


########## STEPWISE WITH CV ################
predict.regsubsets <- function (object , newdata , id) {
  mat <- model.matrix(fatal ~., newdata)
  coefi <- coef (object , id = id)
  xvars <- names (coefi)
  mat[, xvars] %*% coefi
} 

k <- 20
folds <- createFolds(factor(train_df$fatal), k = k, list = FALSE) # this stratifies automatically
cv.errors <- matrix (NA, k, ncol(all_x), dimnames = list(NULL , paste (1:(ncol(all_x)))))
p_area <- matrix (NA, k, ncol(all_x), dimnames = list(NULL , paste (1:(ncol(all_x)))))
for (j in 1:k) {
  best.fit <- regsubsets(fatal ~. ,
                         data = train_df[folds != j, ],
                         nvmax = ncol(all_x), method = "forward", weights = weights[folds != j])
  for (i in 1:(ncol(all_x))) {
    pred <- predict(best.fit , train_df[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean ((train_df$fatal[folds == j] - pred)^2)
    p_area[j, i] <-
      pr.curve(scores.class0 = pred, weights.class0 = train_df$fatal[folds == j], curve = TRUE)$auc.integral
  }
}
mean.cv.errors.forwards <- apply (cv.errors , 2, mean)
mean.p.area.forwards <- apply (p_area , 2, mean)
which.min(mean.cv.errors.forwards)
which.max(mean.p.area.forwards)



cv.errors <- matrix (NA, k, ncol(all_x), dimnames = list(NULL , paste (1:(ncol(all_x)))))
p_area <- matrix (NA, k, ncol(all_x), dimnames = list(NULL , paste (1:(ncol(all_x)))))
for (j in 1:k) {
  best.fit <- regsubsets(fatal ~. ,
                         data = train_df[folds != j, ],
                         nvmax = ncol(all_x), method = "backward", weights = weights[folds != j ])
  for (i in 1:(ncol(all_x))) {
    pred <- predict(best.fit , train_df[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean ((train_df$fatal[folds == j] - pred)^2)
    p_area[j, i] <-
      pr.curve(scores.class0 = pred, weights.class0 = train_df$fatal[folds == j], curve = TRUE)$auc.integral
  }
}
mean.cv.errors.backward <- apply (cv.errors , 2, mean)
mean.p.area.backwards <- apply (p_area , 2, mean)
which.min(mean.cv.errors.backward)
which.max(mean.p.area.backwards)

cv.forward.model <- regsubsets(fatal ~ ., data = non_test_df, nbest = 1, nvmax = ncol(all_x), method = "forward", really.big = TRUE)
cv.backward.model <- regsubsets(fatal ~ ., data = non_test_df, nbest = 1, nvmax = ncol(all_x), method = "backward", really.big = TRUE)


############## CHECKING ########


# names.backward.cp <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1] #
# names.backward.bic <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1] #
# names.forward.cp <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1] #
# names.forward.bic <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1] #
# names.backward.cv <- names(coef(cv.backward.model,which.min(mean.cv.errors.backward)))[-1]  #
# names.forward.cv <- names(coef(cv.forward.model,which.min(mean.cv.errors.forwards)))[-1]  #
# names.forward.p <- names(coef(cv.forward.model,which.max(mean.p.area.forwards)))[-1] #
# names.backward.p <- names(coef(cv.forward.model,which.max(mean.p.area.backwards)))[-1]

# names.backward.cp.5 <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1] # 16.7, 10.3
# names.backward.bic.5 <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1] # 16.84, 10.32
# names.forward.cp.5 <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1] #16.79, 10.314
# names.forward.bic.5 <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1] # 16.83, 10.31
# names.backward.cv.5 <- names(coef(cv.backward.model,which.min(mean.cv.errors.backward)))[-1]  #
# names.forward.cv.5 <- names(coef(cv.forward.model,which.min(mean.cv.errors.forwards)))[-1]  #
# names.forward.p.5 <- names(coef(cv.forward.model,which.max(mean.p.area.forwards)))[-1] #
# names.backward.p.5 <- names(coef(cv.forward.model,which.max(mean.p.area.backwards)))[-1]

# names.backward.cp.10 <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1] #16.59, 10.2
# names.backward.bic.10 <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1] #16.56, 10.21
# names.forward.cp.10 <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1]
# names.forward.bic.10 <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1]
# names.forward.p.10 <- names(coef(cv.forward.model,which.max(mean.p.area.forwards)))[-1] #
# names.backward.p.10 <- names(coef(cv.forward.model,which.max(mean.p.area.backwards)))[-1]

# a.names.backward.cp <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1]
# a.names.backward.bic <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1]
# a.names.forward.cp <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1]
# a.names.forward.bic <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1]
# a.names.backward.cv <- names(coef(cv.backward.model,which.min(mean.cv.errors.backward)))[-1]
# a.names.forward.cv <- names(coef(cv.forward.model,which.min(mean.cv.errors.forwards)))[-1]
# a.names.forward.p <- names(coef(cv.forward.model,which.max(mean.p.area.forwards)))[-1]
# a.names.backward.p <- names(coef(cv.forward.model,which.max(mean.p.area.backwards)))[-1]

# u.names.backward.cp <- names(coef(regfit.backward,which.min(backward.summary$cp)))[-1]
# u.names.backward.bic <- names(coef(regfit.backward,which.min(backward.summary$bic)))[-1]
# u.names.forward.cp <- names(coef(regfit.forward,which.min(forward.summary$cp)))[-1]
# u.names.forward.bic <- names(coef(regfit.forward,which.min(forward.summary$bic)))[-1]
# u.names.backward.cv <- names(coef(cv.backward.model,which.min(mean.cv.errors.backward)))[-1]
u.names.forward.cv <- names(coef(cv.forward.model,which.min(mean.cv.errors.forwards)))[-1]
# u.names.forward.p <- names(coef(cv.forward.model,which.max(mean.p.area.forwards)))[-1]
# u.names.backward.p <- names(coef(cv.forward.model,which.max(mean.p.area.backwards)))[-1]

names <- u.names.forward.cv  

train_model_names <- as.data.frame(model.matrix(fatal ~., train_df))[,-1]
train_model <- train_model_names[, names]
train_model <- cbind(train_model, train_df$fatal)
names(train_model)[ncol(train_model)] <- "fatal"

valid_model <- as.data.frame(model.matrix(fatal ~., valid_df))[,-1]
valid_model <- valid_model[, names]
valid_model <- cbind(valid_model, valid_df$fatal)
names(valid_model)[ncol(valid_model)] <- "fatal"

test_model <- as.data.frame(model.matrix(fatal ~., test_df))[,-1]
test_model <- test_model[, names]
test_model <- cbind(test_model, test_df$fatal)
names(test_model)[ncol(test_model)] <- "fatal"

final_df <- upSample(prediction_df, as.factor(prediction_df$fatal))
final_df$Class = NULL
final_model <- as.data.frame(model.matrix(fatal ~., final_df))[,-1]
final_model <- final_model[, names]
final_model <- cbind(final_model, final_df$fatal)
names(final_model)[ncol(final_model)] <- "fatal"

#Normal logistic regression


weights <- ifelse(train_df$fatal == TRUE, weight, 1)
model <- glm(fatal ~. , data = train_model, family = "binomial", weights = weights)

#summary(model)

#train_model <- significance(model, train_model, 0.01)

pred_valid <- predict(model, newdata = valid_model, type = "response")
pred_top_25_glm <- quantile(pred_valid , 0.75)
cm <- confusionMatrix(data = as.factor(ifelse(as.numeric(pred_valid > pred_top_25_glm),TRUE,FALSE)), reference = as.factor(valid_model$fatal))
cm$byClass
pr_curve <- pr.curve(scores.class0 = pred_valid, weights.class0 = valid_df$fatal, curve = TRUE)
pr_curve$auc.integral
#plot(pr_curve)


thresholds <- seq(0.1, 0.9, by = 0.02)
f1_scores <- vector("numeric", length = length(thresholds))
for (i in seq_along(thresholds)) {
  threshold <- thresholds[i]
  predicted_labels <- ifelse(pred_valid >= threshold, 1, 0)
  
  # Calculate precision, recall, and F1 score
  precision <- sum(predicted_labels == 1 & valid_df$fatal == 1) / sum(predicted_labels == 1)
  recall <- sum(predicted_labels == 1 & valid_df$fatal == 1) / sum(valid_df$fatal == 1)
  f1_scores[i] <- 2 * (precision * recall) / (precision + recall)
}
max(f1_scores, na.rm = TRUE)
which.max(f1_scores)
thresholds[which.max(f1_scores)]


#Lasso and Ridge
x_train <- as.matrix(train_model[,-ncol(train_model)])
y_train <- train_model[,ncol(train_model)]

x_valid <- as.matrix(valid_model[,-ncol(valid_model)])
y_valid <-valid_model[,ncol(valid_model)]

x_total <- rbind(x_train, x_valid)
y_total <- c(y_train, y_valid)

weights <- ifelse(y_train == TRUE, weight, 1)
k <- 20
set.seed(1)
folds <- createFolds(factor(y_train), k = k, list = FALSE) # this stratifies automatically

lasso_model <- cv.glmnet(y = y_train, x = x_train , family = "binomial", foldid = folds, 
                         type.measure = "class", alpha = 1, weights = weights)

lasso_best_lambda <- lasso_model$lambda.min
log(lasso_best_lambda)
lasso_pred_valid <- predict(lasso_model , s = lasso_best_lambda ,
                            newx = x_valid, type = "response")
pred_top_25_lasso <- quantile(lasso_pred_valid , 0.75)
cm <- confusionMatrix(data = as.factor(ifelse(as.numeric(lasso_pred_valid > pred_top_25_lasso),TRUE,FALSE)), reference = as.factor(y_valid))
cm$byClass
pr_curve <- pr.curve(scores.class0 = lasso_pred_valid, weights.class0 = y_valid, curve = TRUE)
pr_curve$auc.integral


k <- 20
set.seed(1)
folds <- createFolds(factor(y_train), k = k, list = FALSE) # this stratifies automatically

ridge_model_train <- cv.glmnet(y = y_train, x = x_train , family = "binomial", foldid = folds, 
                               type.measure = "class", alpha = 0, weights = weights)

ridge_best_lambda <- ridge_model_train$lambda.min
log(ridge_best_lambda)
ridge_pred_valid <- predict(ridge_model_train , s = ridge_best_lambda ,
                            newx = x_valid, type = "response")
pred_top_25_ridge <- quantile(ridge_pred_valid , 0.75)
cm <- confusionMatrix(data = as.factor(ifelse(as.numeric(ridge_pred_valid > pred_top_25_ridge),TRUE,FALSE)), reference = as.factor(y_valid))
cm$byClass
pr_curve <- pr.curve(scores.class0 = ridge_pred_valid, weights.class0 = y_valid, curve = TRUE)
pr_curve$auc.integral

ridge_se_lambda <- ridge_model_train$lambda.1se
log(ridge_se_lambda)
ridge_pred_valid <- predict(ridge_model_train , s = ridge_se_lambda ,
                            newx = x_valid, type = "response")
pred_top_25_ridge <- quantile(ridge_pred_valid , 0.75)
cm <- confusionMatrix(data = as.factor(ifelse(as.numeric(ridge_pred_valid > pred_top_25_ridge),TRUE,FALSE)), reference = as.factor(y_valid))
cm$byClass
pr_curve <- pr.curve(scores.class0 = ridge_pred_valid, weights.class0 = y_valid, curve = TRUE)
pr_curve$auc.integral








#KNN
knn_train <- model.matrix(fatal ~., train_df)[,-1]
knn_train <- scale(knn_train)

knn_valid <- model.matrix(fatal ~., valid_df)[,-1]
knn_valid <- scale(knn_valid)

knn.pred <- knn(knn_train, knn_valid, y_train , k = 1)
cm <- table(y_valid , knn.pred)
precision <- cm[2,2]/(cm[2,2]+cm[2,1])
recall <- cm[2,2]/(cm[2,2]+cm[1,2])
f1 <- 2*precision*recall/(precision+recall)





####### TREES ########


library(rpart)
library(rpart.plot)
library(tree)

# Reformat for tree friendly

tree_train <- train_df
tree_valid <- valid_df
tree_train$fatal <- ifelse(train_df$fatal == TRUE, "Yes", "No")
tree_train$fatal <- as.factor(tree_train$fatal)
tree_valid$fatal <- ifelse(valid_df$fatal == TRUE, "Yes", "No")
tree_valid$fatal <- as.factor(tree_valid$fatal)

weight <- 1
weights <- ifelse(tree_train$fatal == "Yes", weight, 1)

# Control the complexity parameter
model_control = rpart.control(xval = 10, minsplit = 12, cp = 0.001)

tree_model = rpart(fatal ~.,control = model_control, data = tree_train, method = "class", weights = weights)
plotcp(tree_model)
plot(tree_model)
pred_tree <- predict(tree_model, newdata = valid_df, type = "prob")[, 2]
pred_top_25_prune <- quantile(pred_tree , 0.75)
cm_tree <- confusionMatrix(data = as.factor(ifelse(as.numeric(pred_tree > pred_top_25_prune),"Yes","No")), reference = as.factor(tree_valid$fatal))
cm_tree$byClass
pr_curve <- pr.curve(scores.class0 = pred_tree, weights.class0 = valid_df$fatal, curve = TRUE)
pr_curve$auc.integral



###### Bagging / RF ##########


library(randomForestSRC)
library(randomForest)

#RF
weight <- 1
weights <- ifelse(tree_train$fatal == "Yes", weight, 1)
bag_model <- randomForest(fatal ~ . , data = tree_train, importance = T, weights = weights)
pred_bag <- predict(bag_model, newdata = tree_valid, type = "prob")[,2]
pred_top_25_bag <- quantile(pred_bag , 0.75)
cm_bag <- confusionMatrix(data = as.factor(ifelse(as.numeric(pred_bag > pred_top_25_bag),"Yes","No")), reference = as.factor(tree_valid$fatal))
cm_bag$byClass
pr_curve <- pr.curve(scores.class0 = pred_bag, weights.class0 = valid_df$fatal, curve = TRUE)
pr_curve$auc.integral


# Bagging
weight <- 1
weights <- ifelse(tree_train$fatal == "Yes", weight, 1)
bag_model <- randomForest(fatal ~ . , data = tree_train, importance = T)
pred_bag <- predict(bag_model, newdata = tree_valid, type = "prob", mrtry = ncol(all_x))[,2]
pred_top_25_bag <- quantile(pred_bag , 0.75)
cm_bag <- confusionMatrix(data = as.factor(ifelse(as.numeric(pred_bag > pred_top_25_bag),"Yes","No")), reference = as.factor(tree_valid$fatal))
cm_bag$byClass
pr_curve <- pr.curve(scores.class0 = pred_bag, weights.class0 = valid_df$fatal, curve = TRUE)
pr_curve$auc.integral


# Balanced Random Forest
balanced_forest <- rfsrc(fatal ~ ., data = tree_train, classwt = "balanced", splitrule="gini")
pred_balancedforest <- predict(balanced_forest, newdata = tree_valid, type = "prob")
pred_top_25_bfr <- quantile(pred_balancedforest , 0.75)
cm_bfr <- confusionMatrix(data = as.factor(ifelse(as.numeric(pred_balancedforest > pred_top_25_bfr),"Yes","No")), reference = as.factor(tree_valid$fatal))
cm_bfr$byClass
pr_curve <- pr.curve(scores.class0 = pred_balancedforest, weights.class0 = valid_df$fatal, curve = TRUE)
pr_curve$auc.integral


# Boosting

library(gbm)
tree_train$random <- rnorm(nrow(tree_train), 0, 1)

weight <- 1
weights <- ifelse(tree_train$fatal == "Yes", weight, 1)

tree_train$fatal <- ifelse(tree_train$fatal == "Yes", 1, 0)
boost_model <- gbm(fatal ~., data = tree_train, distribution = "bernoulli",
                   n.trees = 500, interaction.depth = 3, shrinkage = 0.01, class.stratify.cv = TRUE, weights = weights)

#summary(boost_model)

pred_boost <- predict(boost_model, newdata = valid_df, type = "response")
pred_top_25_boost <- quantile(pred_boost , 0.75)
cm_boost <- confusionMatrix(data = as.factor(ifelse(as.numeric(pred_boost > pred_top_25_boost),"Yes","No")), reference = as.factor(tree_valid$fatal))
cm_boost$byClass
pr_curve <- pr.curve(scores.class0 = pred_boost, weights.class0 = valid_df$fatal, curve = TRUE)
pr_curve$auc.integral



# TEST TIME - RIDGE WAS CHOSEN
k <- 20
set.seed(1)
folds <- createFolds(factor(y_total), k = k, list = FALSE) # this stratifies automatically

ridge_model_total <- cv.glmnet(y = y_total, x = x_total , family = "binomial", foldid = folds, 
                               type.measure = "class", alpha = 0)
ridge_best_lambda <- ridge_model_total$lambda.min

x_test <- as.matrix(test_model[,-ncol(test_model)])
y_test <-test_model[,ncol(test_model)]

ridge_pred_test <- predict(ridge_model_final , s = ridge_best_lambda ,
                           newx = x_test, type = "response")

# pred_top_25_ridge <- quantile(ridge_pred_test , 0.75)
# cm <- confusionMatrix(data = as.factor(ifelse(as.numeric(ridge_pred_test > pred_top_25_ridge),TRUE,FALSE)), reference = as.factor(y_test))
# cm$byClass
# pr_curve <- pr.curve(scores.class0 = ridge_pred_valid, weights.class0 = y_valid, curve = TRUE)
# pr_curve$auc.integral

x_final <- as.matrix(final_model[,-ncol(final_model)])
y_final <-final_model[,ncol(final_model)]

k <- 20
set.seed(1)
folds <- createFolds(factor(y_final), k = k, list = FALSE) # this stratifies automatically
ridge_model_final <- cv.glmnet(y = y_final, x = x_final , family = "binomial", foldid = folds, 
                               type.measure = "class", alpha = 0)
final_best_lambda <- ridge_model_final$lambda.min








#### PREDICTION TIME #######

eval <- read.csv("Drivers_Eval.csv", stringsAsFactors = TRUE)
summary(eval)

eval_pred <- select(eval, -c(VEHICLE_ID, DRIVER_ID, AGE))

eval_pred$REGION <- ifelse(eval_pred$OWNER_POSTCODE %in% c(3350, 3355, 3356, 3358, 3353, 3354), "Ballarat", 
                           ifelse(eval_pred$OWNER_POSTCODE %in% c(3214, 3215, 3216, 3219, 3220), "Geelong", 
                                  ifelse(eval_pred$OWNER_POSTCODE %in% c(3550, 3552, 3555, 3556, 3005, 3010, 3050, 3086, 3138, 3198, 3199, 3200, 3201, 3800, 3805, 3910, 3930, 3116, 3140, 3338, 3340, 3429, 3806, 3807, 3809, 3810, 3911, 3913, 3915, 3931, 3934, 3938, 3940, 3941, 3942, 3943, 3944, 3024, 3089, 3091, 3096, 3097, 3099, 3115, 3154, 3158, 3159, 3160, 3164, 3765, 3793, 3796, 3804, 3977, 3100, 3336, 3339, 3436, 3439, 3443, 3768, 3769, 3771, 3772, 3773, 3774, 3776, 3780, 3784, 3790, 3794, 3798, 3811, 3914, 3917, 3932, 3935, 3982, 3983), "Melbourne",
                                         ifelse(eval_pred$OWNER_POSTCODE %in% c(3000, 3002, 3003, 3004, 3006, 3008, 3011, 3012, 3013, 3015, 3016, 3018, 3019, 3020, 3021, 3022, 3023, 3025, 3026, 3027, 3028, 3029, 3030, 3031, 3032, 3033, 3034, 3036, 3037, 3038, 3039, 3040, 3041, 3042, 3043, 3044, 3045, 3046, 3047, 3048, 3049, 3051, 3052, 3053, 3054, 3055, 3056, 3057, 3058, 3059, 3060, 3061, 3062, 3063, 3064, 3065, 3066, 3067, 3068, 3070, 3071, 3072, 3073, 3074, 3075, 3076, 3078, 3079, 3081, 3082, 3083, 3084, 3085, 3087, 3088, 3090, 3093, 3094, 3095, 3101, 3102, 3103, 3104, 3105, 3106, 3107, 3108, 3109, 3111, 3113, 3114, 3121, 3122, 3123, 3124, 3125, 3126, 3127, 3128, 3129, 3130, 3131, 3132, 3133, 3134, 3135, 3136, 3137, 3141, 3142, 3143, 3144, 3145, 3146, 3147, 3148, 3149, 3150, 3151, 3152, 3153, 3155, 3156, 3161, 3162, 3163, 3165, 3166, 3167, 3168, 3169, 3170, 3171, 3172, 3173, 3174, 3175, 3177, 3178, 3179, 3180, 3181, 3182, 3183, 3184, 3185, 3186, 3187, 3188, 3189, 3190, 3191, 3192, 3193, 3194, 3195, 3196, 3197, 3202, 3204, 3205, 3206, 3207, 3428, 3752, 3802, 3803, 3975, 3976, 3001, 3007, 3009, 3014, 3017, 3035, 3069, 3077, 3080, 3092, 3098, 3110, 3112, 3117, 3118, 3119, 3120, 3157, 3176, 3203, 3208, 3209, 3210, 3801), "Melbourne Metro",
                                                ifelse(eval_pred$OWNER_POSTCODE %in% c(3218, 3224, 3912, 3936, 3939, 3551, 3139, 3211, 3212, 3213, 3217, 3221, 3222, 3223, 3225, 3226, 3227, 3228, 3230, 3231, 3232, 3335, 3337, 3341, 3342, 3345, 3351, 3352, 3357, 3360, 3361, 3427, 3430, 3431, 3432, 3433, 3434, 3435, 3437, 3438, 3440, 3441, 3442, 3444, 3446, 3447, 3448, 3450, 3451, 3453, 3458, 3460, 3461, 3693, 3750, 3751, 3753, 3754, 3755, 3756, 3757, 3758, 3759, 3760, 3761, 3762, 3763, 3764, 3766, 3767, 3770, 3775, 3777, 3778, 3779, 3781, 3782, 3783, 3785, 3786, 3787, 3788, 3789, 3791, 3792, 3795, 3797, 3799, 3808, 3812, 3813, 3814, 3815, 3816, 3818, 3916, 3918, 3919, 3920, 3921, 3922, 3923, 3925, 3926, 3927, 3928, 3929, 3933, 3937, 3945, 3946, 3950, 3951, 3978, 3979, 3980, 3981, 3984, 3987, 3988, 3990, 3991, 3992, 3995, 3996, 3691, 3233, 3234, 3235, 3236, 3237, 3238, 3239, 3240, 3241, 3242, 3243, 3249, 3250, 3251, 3254, 3260, 3264, 3265, 3266, 3267, 3268, 3269, 3270, 3271, 3272, 3273, 3274, 3275, 3276, 3277, 3278, 3279, 3280, 3281, 3282, 3283, 3284, 3285, 3286, 3287, 3289, 3292, 3293, 3294, 3300, 3301, 3302, 3303, 3304, 3305, 3309, 3310, 3311, 3312, 3314, 3315, 3317, 3318, 3319, 3321, 3322, 3323, 3324, 3325, 3328, 3329, 3330, 3331, 3332, 3333, 3334, 3363, 3364, 3370, 3371, 3373, 3374, 3375, 3377, 3378, 3379, 3380, 3381, 3384, 3385, 3387, 3388, 3390, 3391, 3392, 3393, 3395, 3396, 3399, 3400, 3401, 3407, 3409, 3412, 3413, 3414, 3415, 3418, 3419, 3420, 3422, 3423, 3424, 3462, 3463, 3464, 3465, 3467, 3468, 3469, 3472, 3475, 3477, 3478, 3480, 3482, 3483, 3485, 3487, 3488, 3489, 3490, 3491, 3494, 3496, 3498, 3500, 3501, 3505, 3506, 3507, 3509, 3512, 3515, 3516, 3517, 3518, 3520, 3521, 3522, 3523, 3525, 3527, 3529, 3530, 3531, 3533, 3537, 3540, 3542, 3544, 3546, 3549, 3557, 3558, 3559, 3561, 3562, 3563, 3564, 3565, 3566, 3567, 3568, 3570, 3571, 3572, 3573, 3575, 3576, 3579, 3580, 3581, 3583, 3584, 3585, 3586, 3588, 3589, 3590, 3591, 3594, 3595, 3596, 3597, 3599, 3607, 3608, 3610, 3612, 3614, 3616, 3617, 3618, 3620, 3621, 3622, 3623, 3624, 3629, 3630, 3631, 3633, 3634, 3635, 3636, 3637, 3638, 3639, 3640, 3641, 3644, 3646, 3647, 3649, 3658, 3659, 3660, 3662, 3663, 3664, 3665, 3666, 3669, 3670, 3672, 3673, 3675, 3677, 3678, 3682, 3683, 3685, 3687, 3688, 3695, 3697, 3698, 3699, 3700, 3701, 3704, 3705, 3707, 3708, 3709, 3711, 3712, 3713, 3714, 3715, 3717, 3718, 3719, 3720, 3722, 3723, 3725, 3726, 3727, 3728, 3730, 3732, 3733, 3735, 3737, 3738, 3739, 3740, 3741, 3744, 3746, 3747, 3749, 3820, 3821, 3822, 3823, 3824, 3825, 3831, 3832, 3833, 3835, 3840, 3842, 3844, 3847, 3850, 3851, 3852, 3854, 3856, 3857, 3858, 3859, 3860, 3862, 3864, 3865, 3869, 3870, 3871, 3873, 3874, 3875, 3878, 3880, 3882, 3885, 3886, 3887, 3888, 3889, 3890, 3891, 3892, 3893, 3895, 3896, 3898, 3900, 3902, 3903, 3904, 3909, 3953, 3954, 3956, 3957, 3958, 3959, 3960, 3962, 3964, 3965, 3966, 3967, 3971, 3229, 3244, 3245, 3246, 3247, 3248, 3252, 3253, 3255, 3256, 3257, 3258, 3259, 3261, 3262, 3263, 3288, 3290, 3291, 3295, 3296, 3297, 3298, 3299, 3306, 3307, 3308, 3313, 3316, 3320, 3326, 3327, 3343, 3344, 3346, 3347, 3348, 3349, 3359, 3362, 3365, 3366, 3367, 3368, 3369, 3372, 3376, 3382, 3383, 3386, 3389, 3394, 3397, 3398, 3402, 3403, 3404, 3405, 3406, 3408, 3410, 3411, 3416, 3417, 3421, 3425, 3426, 3445, 3449, 3452, 3454, 3455, 3456, 3457, 3459, 3466, 3470, 3471, 3473, 3474, 3476, 3479, 3481, 3484, 3486, 3492, 3493, 3495, 3497, 3499, 3502, 3503, 3504, 3508, 3510, 3511, 3513, 3514, 3519, 3524, 3526, 3528, 3532, 3534, 3535, 3536, 3538, 3539, 3541, 3543, 3545, 3547, 3548, 3553, 3554, 3560, 3569, 3574, 3577, 3578, 3582, 3587, 3592, 3593, 3598, 3600, 3601, 3602, 3603, 3604, 3605, 3606, 3609, 3611, 3613, 3615, 3619, 3625, 3626, 3627, 3628, 3632, 3642, 3643, 3645, 3648, 3650, 3651, 3652, 3653, 3654, 3655, 3656, 3657, 3661, 3667, 3668, 3671, 3674, 3676, 3679, 3680, 3681, 3684, 3686, 3692, 3696, 3702, 3703, 3706, 3710, 3716, 3721, 3724, 3729, 3731, 3734, 3736, 3742, 3743, 3745, 3748, 3817, 3819, 3826, 3827, 3828, 3829, 3830, 3834, 3836, 3837, 3838, 3839,
                                                                                       3841, 3843, 3845, 3846, 3848, 3849, 3853, 3855, 3861, 3863, 3866, 3867, 3868, 3872, 3876, 3877, 3879, 3881, 3883, 3884, 3894, 3897, 3899, 3901, 3905, 3906, 3907, 3908, 3924, 3947, 3948, 3949, 3952, 3955, 3961, 3963, 3968, 3969, 3970, 3972, 3973, 3974, 3985, 3986, 3989, 3993, 3994, 3997, 3998, 3999), "VIC Country",
                                                       ifelse(eval_pred$OWNER_POSTCODE %in% c(3689, 3690, 3218, 3224, 3912, 3936, 3939, 3551, 3139, 3211, 3212, 3213, 3217, 3221, 3222, 3223, 3225, 3226, 3227, 3228, 3230, 3231, 3232, 3335, 3337, 3341, 3342, 3345, 3351, 3352, 3357, 3360, 3361, 3427, 3430, 3431, 3432, 3433, 3434, 3435, 3437, 3438, 3440, 3441, 3442, 3444, 3446, 3447, 3448, 3450, 3451, 3453, 3458, 3460, 3461, 3693, 3750, 3751, 3753, 3754, 3755, 3756, 3757, 3758, 3759, 3760, 3761, 3762, 3763, 3764, 3766, 3767, 3770, 3775, 3777, 3778, 3779, 3781, 3782, 3783, 3785, 3786, 3787, 3788, 3789, 3791, 3792, 3795, 3797, 3799, 3808, 3812, 3813, 3814, 3815, 3816, 3818, 3916, 3918, 3919, 3920, 3921, 3922, 3923, 3925, 3926, 3927, 3928, 3929, 3933, 3937, 3945, 3946, 3950, 3951, 3978, 3979, 3980, 3981, 3984, 3987, 3988, 3990, 3991, 3992, 3995, 3996, 3691, 3233, 3234, 3235, 3236, 3237, 3238, 3239, 3240, 3241, 3242, 3243, 3249, 3250, 3251, 3254, 3260, 3264, 3265, 3266, 3267, 3268, 3269, 3270, 3271, 3272, 3273, 3274, 3275, 3276, 3277, 3278, 3279, 3280, 3281, 3282, 3283, 3284, 3285, 3286, 3287, 3289, 3292, 3293, 3294, 3300, 3301, 3302, 3303, 3304, 3305, 3309, 3310, 3311, 3312, 3314, 3315, 3317, 3318, 3319, 3321, 3322, 3323, 3324, 3325, 3328, 3329, 3330, 3331, 3332, 3333, 3334, 3363, 3364, 3370, 3371, 3373, 3374, 3375, 3377, 3378, 3379, 3380, 3381, 3384, 3385, 3387, 3388, 3390, 3391, 3392, 3393, 3395, 3396, 3399, 3400, 3401, 3407, 3409, 3412, 3413, 3414, 3415, 3418, 3419, 3420, 3422, 3423, 3424, 3462, 3463, 3464, 3465, 3467, 3468, 3469, 3472, 3475, 3477, 3478, 3480, 3482, 3483, 3485, 3487, 3488, 3489, 3490, 3491, 3494, 3496, 3498, 3500, 3501, 3505, 3506, 3507, 3509, 3512, 3515, 3516, 3517, 3518, 3520, 3521, 3522, 3523, 3525, 3527, 3529, 3530, 3531, 3533, 3537, 3540, 3542, 3544, 3546, 3549, 3557, 3558, 3559, 3561, 3562, 3563, 3564, 3565, 3566, 3567, 3568, 3570, 3571, 3572, 3573, 3575, 3576, 3579, 3580, 3581, 3583, 3584, 3585, 3586, 3588, 3589, 3590, 3591, 3594, 3595, 3596, 3597, 3599, 3607, 3608, 3610, 3612, 3614, 3616, 3617, 3618, 3620, 3621, 3622, 3623, 3624, 3629, 3630, 3631, 3633, 3634, 3635, 3636, 3637, 3638, 3639, 3640, 3641, 3644, 3646, 3647, 3649, 3658, 3659, 3660, 3662, 3663, 3664, 3665, 3666, 3669, 3670, 3672, 3673, 3675, 3677, 3678, 3682, 3683, 3685, 3687, 3688, 3695, 3697, 3698, 3699, 3700, 3701, 3704, 3705, 3707, 3708, 3709, 3711, 3712, 3713, 3714, 3715, 3717, 3718, 3719, 3720, 3722, 3723, 3725, 3726, 3727, 3728, 3730, 3732, 3733, 3735, 3737, 3738, 3739, 3740, 3741, 3744, 3746, 3747, 3749, 3820, 3821, 3822, 3823, 3824, 3825, 3831, 3832, 3833, 3835, 3840, 3842, 3844, 3847, 3850, 3851, 3852, 3854, 3856, 3857, 3858, 3859, 3860, 3862, 3864, 3865, 3869, 3870, 3871, 3873, 3874, 3875, 3878, 3880, 3882, 3885, 3886, 3887, 3888, 3889, 3890, 3891, 3892, 3893, 3895, 3896, 3898, 3900, 3902, 3903, 3904, 3909, 3953, 3954, 3956, 3957, 3958, 3959, 3960, 3962, 3964, 3965, 3966, 3967, 3971, 3229, 3244, 3245, 3246, 3247, 3248, 3252, 3253, 3255, 3256, 3257, 3258, 3259, 3261, 3262, 3263, 3288, 3290, 3291, 3295, 3296, 3297, 3298, 3299, 3306, 3307, 3308, 3313, 3316, 3320, 3326, 3327, 3343, 3344, 3346, 3347, 3348, 3349, 3359, 3362, 3365, 3366, 3367, 3368, 3369, 3372, 3376, 3382, 3383, 3386, 3389, 3394, 3397, 3398, 3402, 3403, 3404, 3405, 3406, 3408, 3410, 3411, 3416, 3417, 3421, 3425, 3426, 3445, 3449, 3452, 3454, 3455, 3456, 3457, 3459, 3466, 3470, 3471, 3473, 3474, 3476, 3479, 3481, 3484, 3486, 3492, 3493, 3495, 3497, 3499, 3502, 3503, 3504, 3508, 3510, 3511, 3513, 3514, 3519, 3524, 3526, 3528, 3532, 3534, 3535, 3536, 3538, 3539, 3541, 3543, 3545, 3547, 3548, 3553, 3554, 3560, 3569, 3574, 3577, 3578, 3582, 3587, 3592, 3593, 3598, 3600, 3601, 3602, 3603, 3604, 3605, 3606, 3609, 3611, 3613, 3615, 3619, 3625, 3626, 3627, 3628, 3632, 3642, 3643, 3645, 3648, 3650, 3651, 3652, 3653, 3654, 3655, 3656, 3657, 3661, 3667, 3668, 3671, 3674, 3676, 3679, 3680, 3681, 3684, 3686, 3692, 3696, 3702, 3703, 3706, 3710, 3716, 3721, 3724, 3729, 3731, 3734, 3736, 3742, 3743, 3745, 3748, 3817, 3819, 3826, 3827, 3828, 3829, 3830, 3834, 3836, 3837, 
                                                                                              3838,
                                                                                              3839, 3841, 3843, 3845, 3846, 3848, 3849, 3853, 3855, 3861, 3863, 3866, 3867, 3868, 3872, 3876, 3877, 3879, 3881, 3883, 3884, 3894, 3897, 3899, 3901, 3905, 3906, 3907, 3908, 3924, 3947, 3948, 3949, 3952, 3955, 3961, 3963, 3968, 3969, 3970, 3972, 3973, 3974, 3985, 3986, 3989, 3993, 3994, 3997, 3998, 3999), "Wodonga", "Other"))))))

eval_pred$REGION <- as.factor(eval_pred$REGION)

eval_pred <- select(eval_pred, -c(OWNER_POSTCODE))

ridge_eval <- as.data.frame(model.matrix(~., eval_pred))[,-1]
ridge_eval <- ridge_eval[, names]
ridge_eval <- as.matrix(ridge_eval)

final_predict <- predict(ridge_model_final , s = final_best_lambda ,
                         newx = ridge_eval, type = "response")
eval_pred <- eval_pred %>% mutate(probs = final_predict)
eval_pred <- eval_pred %>% mutate(predicted = ifelse(final_predict > quantile(final_predict, 0.75), TRUE, FALSE))
eval <- eval %>% mutate(probs = final_predict)
eval <- eval %>% mutate(predicted = ifelse(final_predict > quantile(final_predict, 0.75), TRUE, FALSE))
eval_pred_true <- eval_pred[eval_pred$predicted == TRUE,]

table(eval_pred$SEX)
table(eval_pred_true$SEX)
table(eval_pred_true$SEX)/(table(eval_pred$SEX) + table(eval_pred_true$SEX))
table(eval_pred_true$Age.Group)
table(eval_pred$HELMET_BELT_WORN)
table(eval_pred_true$HELMET_BELT_WORN)
table(eval_pred$REGION)
table(eval_pred_true$REGION)
table(eval_pred$VEHICLE_YEAR_MANUF)
table(eval_pred_true$VEHICLE_YEAR_MANUF)
table(eval_pred$VEHICLE_BODY_STYLE)
table(eval_pred_true$VEHICLE_BODY_STYLE)
table(eval_pred$FUEL_TYPE)
table(eval_pred_true$FUEL_TYPE)

create_combined_pie_chart(eval_pred)
create_combined_pie_chart(eval_pred_true)


write.csv(eval, "final_predictions.csv")

