# DSST Scheduler App 
## An application for assigning students to a course schedule. Created for the amazing teachers at the Denver School of Science and Technology.
### Overview
* The purpose of this app is to provide a quick way to assign students to classes given the constraints of course availabilities. It is also designed to handle paired sections, that is instances where a student must be in Class B given their enrollment in Class A. 
* Data ingestion is handled via an excel file which contains three tabs:
  + ```student_data:``` the table which contains all relevant info on the students course requirements
  + ```schedule_data:``` the table which contains relevant info on the course offerings. For the purposes of this application, I define the following relationships:
      + Subject: One of the basic types of learning that a student needs (e.g., math, science, reading, etc.)
        + Class: Given a subject, the particular course material that student needs (e.g. Algebra 1, Pre-Calc, etc.)
          + Section: A unique offering of a class defined by a period, and teacher. The lowest level of granularity of this data.
  + ```paired_sections:``` the table which defines instances of common requirements (e.g., if a student is in section a Chemistry Class, they also need a lab period). 
* This code is deployable in GUI format via a shiny app. This app is deployed [here](https://john-stevenson.shinyapps.io/dsst_schedule_builder/).  

_Disclaimer: This app is still a work in progress. Please create an issue as you encounter them._
