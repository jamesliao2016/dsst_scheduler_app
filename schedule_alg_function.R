# packages needed ####
library(dplyr)
library(xlsx)

#set directory containing needed data ####
setwd("~/Box Sync/lela_class_schedule/")

#grab student and schedule data as two separate data frames (tables) ####
student_data = read.xlsx2(file = "student_schedule_10_blank.xlsx",
                          sheetName = "student_data",
                          stringsAsFactors = F)
schedule_data = read.xlsx2(file = "student_schedule_10_blank.xlsx",
                           sheetName = "schedule_data",
                           stringsAsFactors = F)
paired_sections = read.xlsx2(file = "student_schedule_10_blank.xlsx",
                             sheetName = "paired_sections",
                             stringsAsFactors = F)

section_capacity = 35
scheduler_10 <- function(student_data_var, schedule_data_var, paired_sections_var, section_capacity = 35) {
# summarize the schedule order by number of sections, ####
# this will inform the order of assignment starting from fewest to most
  
  student_data = student_data_var
  schedule_data = schedule_data_var
  paired_sections = paired_sections_var
  
  
  schedule_order = schedule_data %>%
  select(class, section_id) %>%
  group_by(class) %>%
  count(class, sort = T) %>%
  arrange(n)
colnames(schedule_order) <- c("class", "class_ct")

#changing metadata as needed####

student_data$student_id <- as.numeric(student_data$student_id)
student_data$reading_level <- as.numeric(student_data$reading_level)
student_data$math_level <- as.numeric(student_data$math_level)

schedule_data$section_id <- as.numeric(schedule_data$section_id)
schedule_data$period <- as.numeric(schedule_data$period)

schedule_order$class <- as.character(schedule_order$class)

paired_sections$section_id <- as.numeric(paired_sections$section_id)

paired_sections$paired_section_id <-
  as.numeric(paired_sections$paired_section_id)

# adding a capacity value for each section
schedule_data$section_capacity = section_capacity
# create a list of students ordered by combined reading level and math level####
student_list_df <-
  student_data %>% select(student_id, reading_level, math_level) %>%
  mutate(combined_level = reading_level + math_level) %>% arrange(combined_level, student_id)
student_list_df$join_col = 1 # for cross joining with items below

# create a comprehensive list of student-period, student-subject combos with section_id for filling in below####
#student-subject data frame
subject_df <-
  data.frame(as.character(levels(as.factor(
    schedule_data$subject
  ))), 1)
colnames(subject_df) <- c("subject", "join_col")
student_subject_df <-
  inner_join(student_list_df, subject_df) %>% select(student_id, subject)
student_subject_df$section_id <- 0

# student-period data frame
period_df <-
  data.frame(as.character(levels(as.factor(
    schedule_data$period
  ))), 1)
colnames(period_df) <- c("period", "join_col")
student_period_df <-
  inner_join(student_list_df, period_df) %>% select(student_id, period)
student_period_df$section_id <- 0
student_period_df$period <- as.character(student_period_df$period)
student_period_df$period <- as.numeric(student_period_df$period)
for (i in 1:nrow(schedule_order)) {
  Sys.sleep('0.1')
  subject_out =  schedule_data %>% filter(class %in% schedule_order$class[i]) %>% select(subject)
  subject_out = max(subject_out$subject)
  student_without_this_subject = student_subject_df  %>% filter(subject == subject_out) %>% filter(section_id == 0) %>% select(student_id)
  
  
  schedule_data %>% filter(class == schedule_order$class[i]) %>% select(subject)
  # get the student ids with the given class assignment ####
  student_list = student_data %>% filter(
    science_class == schedule_order$class[i] |
      math_class == schedule_order$class[i] |
      spanish_class == schedule_order$class[i] |
      eng_class == schedule_order$class[i] |
      hist_class == schedule_order$class[i]
  ) %>% filter(student_id %in% student_without_this_subject$student_id) %>%
    select (student_id)
  student_list <- student_list$student_id # coerce to vector
  
  # create a subject field to sort by below ####
  subject_level_sort <-
    schedule_data %>% filter(class == schedule_order$class[i]) %>% select(subject)
  subject_level_sort <- max(subject_level_sort$subject)
  
  # sort based on subject and subset student_list_df by the list of students in student_list ####
  if (subject_level_sort == 'Math') {
    student_list_df <- student_list_df %>% arrange(math_level)
  }
  if (subject_level_sort == 'English') {
    student_list_df <- student_list_df %>% arrange(reading_level)
  }
  if (subject_level_sort != 'English' &
      subject_level_sort != 'Math') {
    student_list_df <- student_list_df %>% arrange(combined_level)
  }
  
  # get the section_ids for the given class assignment ####
  section_list <-
    schedule_data %>% filter(class == schedule_order$class[i])
  
  # if there are any paired sections in this section list, bulk assign them now
  # then remove that section from the section list
  #
  
  # round robin through the students ordered by student performance ####
  for (j in 1:length(student_list)) {
    Sys.sleep('0.1')
    
    section_list <- #grab the sections with capacity
      schedule_data %>% filter(class == schedule_order$class[i])  %>%
      filter (section_capacity > 0)
    
    student_free_periods = student_period_df %>% #grab students free periods
      filter(student_id == student_list[j] &
               section_id == 0)
    
    student_free_periods$period <- # coerce to char
      as.character(student_free_periods$period)
    student_free_periods$period <- # coerce to num for join
      as.numeric(student_free_periods$period)
    
    student_section_overlap = inner_join(section_list, # join the section list and the student free periods
                                         student_free_periods,
                                         by = 'period')
    
    # get the paired sections that are part of the section overlap
    paired_section_overlap = paired_sections %>% filter(section_id %in% student_section_overlap$section_id.x)
    
    paired_section_overlap$paired_section_id <-
      as.numeric(paired_section_overlap$paired_section_id)
    
    # grab the student class list from student data
    student_class_list = student_data %>% filter(student_id == student_list[j])
    
    # return the relevant paired section data so that we can use it for matching
    pair_section_overlap_sched = schedule_data %>%
      filter(section_id %in% paired_section_overlap$paired_section_id) %>%
      filter(class %in% student_class_list)
    
    #joining paired_section_overlap to pair_section_overlap_sched so that we
    #implicitly filter to the class type that s/he needs (i.e. get rid of
    # spanish 1 if they are in spanish 2)
    pair_section_overlap_sched =  inner_join(
      paired_section_overlap,
      pair_section_overlap_sched,
      by = c("paired_section_id" = "section_id")
    )
    #class_student_data = student_data %>% filter(student_id == student_list[j])
    # if (nrow(pair_section_overlap_sched) > 0) {
    if (nrow(pair_section_overlap_sched) == 0 ||
        !(pair_section_overlap_sched$period %in% student_free_periods$period) ||
        !(pair_section_overlap_sched$class %in% student_class_list)) {
      # if the paired section is not in
      # a student's free periods...
      #...then we need to remove the paired section from the student_section_overlap df
      student_section_overlap = student_section_overlap %>%
        filter(!(section_id.x %in% paired_section_overlap$section_id))
    }
    
    #MAY NEED TO REVISIT###########
    if (nrow(pair_section_overlap_sched) > 1 ){
      
      pair_section_overlap_sched <- pair_section_overlap_sched[1,]
    }
    
    # }
    
    if (nrow(student_section_overlap) == 0) {
      # if there aren't any free periods then we need to try to rearrange the current periods.
      # Come up with a list of alternative classes that they could take
      
      student_period_list = student_period_df %>% filter(student_id == student_list[j])
      student_free_periods = student_period_list %>% filter(section_id == 0)
      
      student_period_alternative_list = student_period_list %>% # get the section list from
        inner_join(schedule_data,
                   by = c("section_id" = "section_id", "period" = "period")) %>% #bring in the additional sections
        anti_join(paired_sections, by = c("section_id" = "section_id")) %>% # removing any paired sections, as this seems likely to create problems
        inner_join(
          schedule_data %>% # join to schedule data
            filter(section_capacity > 0) %>% # where there's a seat
            select(section_id, class, period) %>%
            filter(!(
              section_id %in% student_period_list$section_id
            )),
          by = c("class" = "class")
        ) %>%
        inner_join(student_free_periods,
                   by = c("student_id" = "student_id", "period.y" = "period"))
      
      #the previous step established if there were any alternative classes that they could be placed in
      # if so...
      if (nrow(student_period_alternative_list) > 0) {
        # then we need to do some swaps...
        #first constrain this to one row...(we're guaranteed for this to work due to the filtering we did previously)
        student_period_alternative_list <-
          student_period_alternative_list[1,]
        
        # add back the seat to the old section
        schedule_data$section_capacity[which(schedule_data$section_id == student_period_alternative_list$section_id.x)] =
          schedule_data$section_capacity[which(schedule_data$section_id == student_period_alternative_list$section_id.x)] + 1
        
        # remove the seat from the new section
        schedule_data$section_capacity[which(schedule_data$section_id == student_period_alternative_list$section_id.y)] =
          schedule_data$section_capacity[which(schedule_data$section_id == student_period_alternative_list$section_id.y)] - 1
        
        #unassign the student's previous section
        student_period_df$section_id[which(
          student_period_df$student_id == student_period_alternative_list$student_id &
            student_period_df$period == student_period_alternative_list$period.x
        )] = 0 # assign that section id to the std-pd df
        
        student_subject_df$section_id[which(
          student_period_df$student_id == student_period_alternative_list$student_id &
            student_subject_df$subject %in% student_period_alternative_list$subject.0
        )] = 0
        
        # now assign that new section to the student's period and subject list
        student_period_df$section_id[which(
          student_period_df$student_id == student_period_alternative_list$student_id &
            student_period_df$period == student_period_alternative_list$period.y
        )] = student_period_alternative_list$section_id.y # assign that section id to the std-pd df
        
        student_subject_df$section_id[which(
          student_period_df$student_id == student_period_alternative_list$student_id &
            student_subject_df$subject %in% student_period_alternative_list$subject.y
        )] = student_section_overlap$section_id.y
        
      # recalculate the student's free periods
        student_free_periods = student_period_df %>% #grab students free periods
          filter(student_id == student_list[j] &
                   section_id == 0)
        
        student_free_periods$period <- # coerce to char
          as.character(student_free_periods$period)
        student_free_periods$period <- # coerce to num for join
          as.numeric(student_free_periods$period)
        
        student_section_overlap = inner_join(section_list, # join the section list and the student free periods
                                             student_free_periods,
                                             by = 'period')
       
        
      } else {
        #throw an error if there is not space/free period
       stop(message(
          paste(
            'student',
            student_list[j],
            'cannot be scheduled for',
            schedule_order$class[i] ,
            ', because there is not enough space or they do not have a period free...you may want to look at this... '
          )
        ) # TO DO: write student free period and section list df or display them on the screen
       )
      }
      
    }
    
    student_section_overlap <-
      # constrain student sections to those with most capacity
      student_section_overlap %>% filter(section_capacity == max(section_capacity))
    student_section_overlap <-
      student_section_overlap[1, ] #constrain to 1 row
    
    if (nrow(
      student_section_overlap %>% filter(section_id.x %in% pair_section_overlap_sched$section_id)
    ) > 0) {
      #if the one section in the student_section_overlap df has a paired section with it
      # then we need to do a couple of things:
      
      #1. remove the student from any enrolled class that they have for that subject
      #   (i.e. we don't want them getting more than one class of spanish or math or whatever)
      
      former_section =  student_subject_df %>%
        filter(subject == pair_section_overlap_sched$subject) %>%  #get the subject of the paired section
        filter(student_id == student_section_overlap$student_id) %>% # get the student id in question
        select(section_id) # return the section id
      #2. add that capacity back into the schedule_data df for that section
      #3. assign them to that paired section
      #4. reduce the capacity in the paired section by 1
      if (former_section$section_id > 0) {
        # if this section id is not equal to zero, then it means that they have been previously
        # assigned and need to be unassigned
        schedule_data$section_capacity[which(schedule_data$section_id == former_section$section_id)] =
          schedule_data$section_capacity[which(schedule_data$section_id == former_section$section_id)] + 1
      }
      
      
      student_period_df$section_id[which(
        student_period_df$student_id == student_section_overlap$student_id &
          student_period_df$period == pair_section_overlap_sched$period
      )] = pair_section_overlap_sched$paired_section_id # assign that section id to the std-pd df
      
      
      student_subject_df$section_id[which(
        student_period_df$student_id == student_section_overlap$student_id &
          student_subject_df$subject %in% pair_section_overlap_sched$subject
      )] = pair_section_overlap_sched$paired_section_id # assign that section id to the std-sj df
      
      # reduce schedule capacity by 1
      schedule_data$section_capacity[which(schedule_data$section_id == pair_section_overlap_sched$paired_section_id)] =
        schedule_data$section_capacity[which(schedule_data$section_id == pair_section_overlap_sched$paired_section_id)] - 1
      
    }
    
    #set the section id in student_period_df and
    #student_subject_df = to section_id.x from student_section_overlap
    student_period_df$section_id[which(
      student_period_df$student_id == student_section_overlap$student_id &
        student_period_df$period == student_section_overlap$period
    )] = student_section_overlap$section_id.x # assign that section id to the std-pd df
    
    student_subject_df$section_id[which(
      student_period_df$student_id == student_section_overlap$student_id &
        student_subject_df$subject %in% student_section_overlap$subject
    )] = student_section_overlap$section_id.x # assign that section id to the std-sj df
    
    # reduce schedule capacity by 1
    schedule_data$section_capacity[which(schedule_data$section_id == student_section_overlap$section_id.x)] =
      schedule_data$section_capacity[which(schedule_data$section_id == student_section_overlap$section_id.x)] - 1
    
    barplot(
      # plot the results so that we can see them
      schedule_data$section_capacity,
      main = 'Capacity Remaining in Classes',
      beside = T,
      horiz = T,
      names.arg = schedule_data$section_id,
      cex.names = 0.5,
      ylab = "Section ID"
    )
    
    assigned_pct = nrow(student_subject_df %>% filter(section_id > 0)) / nrow(student_subject_df)
    
    print(
      paste(
        "I have assigned",
        round(assigned_pct, 3),
        "of the class...",
        "I'm currently working on",
        schedule_order$class[i]
      )
    )
    
  }
  
}

student_free_periods = student_period_df %>% filter(section_id == 0)

student_free_subjects = student_subject_df %>% filter(section_id == 0)

#student_data %>% inner_join(student_subject_df %>% filter(section_id == 0))

#schedule_out <- list(student_free_periods, student_free_subjects, student_subject_df )

student_schedule_out =  left_join( student_subject_df, schedule_data, by = c("section_id" = "section_id", "subject" = "subject") )

student_out = list(student_free_periods, student_free_subjects, student_schedule_out, schedule_data, paired_sections)

names(student_out) <- c('student_free_periods', 'student_free_subjects', 'student_schedule_out', 'schedule_data', 'paired_sections')

student_out
}



student_out = scheduler_10(student_data_var = student_data, 
                           schedule_data_var = schedule_data, 
                           paired_sections_var = paired_sections, 
                           section_capacity = 35)


names(student_out)[[1]]
write.xlsx2(student_schedule_out, file = "student_schedule_out.xlsx", row.names = F, sheetName = "student_schedule_out", append = F)

write.xlsx2(student_data, file = "student_schedule_out.xlsx", row.names = F, sheetName = "student_data", append = T)

write.xlsx2(schedule_data, file = "student_schedule_out.xlsx", row.names = F, sheetName = "schedule_data", append = T)

write.xlsx2(paired_sections, file = "student_schedule_out.xlsx", row.names = F, sheetName = "paired_sections", append = T)
