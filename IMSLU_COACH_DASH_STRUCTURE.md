# IMSLU Coaching Dashboard - Complete Structure Documentation

## Project Overview
App: Updated coaching dashboard for IMSLU residency program
Repo: fbuckhold3/imslu.coach.dash
Architecture: Based on RDM 2.0 data structure (modeled after fbuckhold3/imslu-resident-self-assessment)
Data Dictionary: rdm2_data_dict_10_22_25.csv (520 rows)
Related Repos: fbuckhold3/gmed (visualization package), fbuckhold3/imslu-resident-self-assessment (architecture model)

## Key Architecture Decisions

### Data Loading
- **Function**: `gmed::load_rdm_complete()`
- **Parameters**: Must use `raw_or_label = "raw"` for checkbox field handling
- **Returns**: List with components:
  - `residents`: Resident demographic and coach assignment data
  - `assessment_data`: Assessment/evaluation data
  - `all_forms`: All form data from REDCap
  - `historical_medians`: Historical comparison data
  - `milestone_data`: Milestone tracking data
  - `data_dict`: REDCap data dictionary
- **Post-processing**: Filters archived residents after calculating historical medians, adds level_at_time calculations

### Period Selection
- **Current Period**: Determined by `gmed` functions from self-assessment app
- **Period Override**: When user changes period, reload ALL data for selected period + previous period (or NA if no previous)
- **Previous Period Data**: Use exact previous period to current (not most recent with data)

### Review Types
- **Primary Coaching Review**: Form = `coach_rev`
- **Second Review**: Form = `second_review`
- **Ad Hoc Reviews**: Separate button (implementation TBD)
- **Instance**: Use `gmed::get_redcap_instance(level, period, review_type = "scheduled")`

## REDCap Forms Structure

### Key Forms
1. **resident_data**: Demographic, coach assignments, test scores, board prep
2. **s_eval**: Self-evaluation (wellness, topics, learning styles, career, board concerns, plus/delta)
3. **ilp**: Individual Learning Plan (goals, action plans, milestone selections)
4. **milestone_selfevaluation_c33c**: Milestone self-assessment with descriptions
5. **milestone_entry**: Coach milestone entry (submitted by this app)
6. **acgme_miles**: ACGME milestone ratings (for comparison)
7. **coach_rev**: Primary coaching review (submitted by this app)
8. **second_review**: Secondary review (submitted by this app)
9. **scholarship**: Scholarship projects and activities
10. **test_data**: ITE scores by PGY year
11. **questions**: Conference questions/attendance
12. **ccc_review**: CCC review notes (for risk/concern status)

### Coach Assignment Fields (resident_data form)
- **coach**: Primary coach dropdown
- **coach_email**: Coach email address
- **second_rev**: Second reviewer name/ID
- **res_archive**: Archive status (yesno)

## Resident Selection Table

### Columns
1. **Name**: From `residents$name` or `residents$first_name` + `residents$last_name`
2. **Level (PGY)**: Current PGY year
3. **Current Period**: Active coaching period
4. **S_eval completed**: Green check if completed, red dot if not
5. **Coach_review completed**: Green check if completed, red dot if not  
6. **Second_review completed**: Green check if completed, red dot if not

### Filter Logic
Show residents where:
- User is primary coach (`coach` field matches current user) OR
- User is second reviewer (`second_rev` field matches current user)

For each record_id in current period (or selected period)

### Completion Status Indicators
- **Green check (✓)**: Form/section completed for this period
- **Red dot (●)**: Form/section not yet completed
- **Logic**: Check for presence of complete data in:
  - s_eval form (form complete status or key fields filled)
  - coach_rev form (coach_rev_complete = "2" or presence of review data)
  - second_review form (second_review_complete = "2" or presence of review data)

## Review Interface - Accordion Sections

### Header (Always Visible)
- Resident name
- PGY year  
- Current period with dropdown override
- Navigation breadcrumbs

---

### Section 1: Wellness & Progress

**Display Previous Period:**
- `s_e_well` (text): "How are you doing mentally"
- `s_e_prog_assist` (text): "Anything program can do?"
- `coach_wellness` (notes): Coach's wellness notes from previous period

**Display Current Period:**
- `s_e_well` (text): Current wellness status
- `s_e_prog_assist` (text): Current program assistance needs

**User Enters (Current Period):**
- `coach_wellness` (notes): Notes about wellness

**Form**: coach_rev

---

### Section 2: Evaluations & Feedback

**Assessment Visualizations** (Using gmed modules):
- `assessment_viz_ui/server`: Main assessment overview
- `mod_assessment_detail_custom_ui/server`: Detailed assessment breakdown
- `mod_cc_completion_ui/server`: Core curriculum completion
- `mod_questions_viz_ui/server`: Conference questions (moved to Section 3)
- `mod_plus_delta_table_ui/server`: Plus/delta table display
- **Exclude**: Conference questions visualization (moved to Learning section)

**Display Current Period:**
- `s_e_plus` (notes): Plus feedback review from resident
- `s_e_delta` (notes): Delta feedback review from resident

**Display Previous Period:**
- `coach_p_d_comments` (notes): Previous coach comments on plus/delta
- `coach_evaluations` (notes): Previous coach comments on evaluations

**User Enters (Current Period):**
- `coach_p_d_comments` (notes): Notes on plus/delta and resident's review of them
- `coach_evaluations` (notes): Notes about completion status of assessments and quality of faculty feedback

**Form**: coach_rev

---

### Section 3: Learning & Board Preparation

**Learning Topics Summary** (Previous + Current Period):
- `s_e_topic_sel` (checkbox): Top 3 topics resident feels least confident about
  - Choices include core IM topics (will be in data dictionary choices field)
- `s_e_topic_oth` (text): Other topic if specified
- **Display**: Table or list showing selected topics from both periods for comparison

**Learning Styles Summary** (Previous + Current Period):
- `s_e_learn_style` (checkbox): Desired learning experiences
- `s_e_learn_oth` (text): Other learning style preferences
- **Display**: Comparison of learning style preferences across periods

**Conference Attendance/Questions Visualization**:
- Use `mod_questions_viz_ui/server` from gmed package
- Data from `questions` form:
  - `q_date` (text): Date of question
  - `q_rotation` (dropdown): Rotation
  - `q_answer` (dropdown): Answer correctness
  - `q_word` (text): Word of the day
  - `q_level` (dropdown): Question level

**ITE Data Display** (from test_data form):
Based on resident's PGY year, display relevant ITE percentiles:
- **PGY1**: `pgy1_total_ile`, `pgy1_cards_ile`, `pgy1_endo_ile`, `pgy1_gi_ile`, `pgy1_gim_ile`, `pgy1_geri_ile`, `pgy1_hemonc_ile`, `pgy1_id_ile`, `pgy1_nephro_ile`, `pgy1_neuro_ile`, `pgy1_pulm_ccm_ile`, `pgy1_rheum_ile`, `pgy1_hvc_ile`
- **PGY2**: `pgy2_*` fields (same subspecialty breakdown)
- **PGY3**: `pgy3_*` fields (same subspecialty breakdown)

**Risk Status**:
- Calculated/derived from CCC review data (ccc_review form)
- `ccc_concern` (yesno): Any concerns of the CCC?
- `ccc_action` (checkbox): Actions suggested by CCC
- Display current risk/concern status prominently

**Step 3 Data** (from resident_data form):
- `step3` (yesno): USMLE and/or COMLEX Step 3 Passed?
- `usmle_step3_failure` (yesno): USMLE Step 3 Failure
- `usmle_step3_score` (text/integer): USMLE Step 3 Score
- `comlex_step3_failure` (yesno): COMLEX Step 3 Failure

**Step 3 Self-Evaluation** (from s_eval form, if Step 3 not yet completed):
- `s_e_step3` (yesno): Have you completed Step 3?
- `s_e_step3_contact` (yesno): Have you emailed your score to the program?
- `s_e_step3_date_set` (yesno): Have you set a date for Step 3?
- `s_e_step3_date` (text): When are you scheduled to take Step 3?

**Board Concerns** (from s_eval form):
- `s_e_board_concern` (yesno): Any concerns about possibly failing boards?
- `s_e_board_help` (yesno): Discussed with program or reached out for help?
- `s_e_board_discu` (notes): Who discussed with and steps being taken?

**User Enters (Current Period):**
- `coach_step_board` (notes): Notes on step completion or board prep

**Form**: coach_rev

---

### Section 4: Scholarship

**Scholarship Display** (Using gmed module):
- `mod_scholarship_display_ui/server`: Table breakdown from gmed package

**Scholarship Form Fields** (scholarship form, repeating):
- `schol_type` (dropdown): Type of project
- `schol_ps` (yesno): Participated in patient safety review?
- `schol_rca` (yesno): Participated in root cause analysis?
- `schol_qi` (notes): QI project description
- `schol_res` (notes): Research project description
- `schol_res_mentor` (notes): Research mentor
- `schol_res_status` (dropdown): Project status
- `schol_pres` (yesno): Presented project?
- `schol_pres_type` (dropdown): Presentation type/location
- `schol_pres_conf` (notes): Conference name
- `schol_cit` (notes): Full citation
- `schol_pub` (yesno): Published?
- `schol_comm` (notes): Committee name
- `schol_comm_type` (dropdown): Committee affiliation
- `schol_comm_other` (notes): Other committee info
- `schol_div` (dropdown): Division

**Display Previous Period:**
- `coach_scholarship` (notes): Previous scholarship comments

**User Enters (Current Period):**
- `coach_scholarship` (notes): Notes on scholarship

**Form**: coach_rev

---

### Section 5: Career Planning

**Career Path** (from s_eval form):
Display checked boxes/selections from current period:
- `s_e_career_path` (checkbox): Career path(s) being considered
- `s_e_career_oth` (text): Other career path

**Fellowship Interests**:
- `s_e_fellow` (checkbox): Fellowship(s) being considered
- `s_e_fellow_oth` (text): Other fellowship

**Track Participation**:
- `s_e_track` (yesno): Planning to pursue program tracks?
- `s_e_track_type` (checkbox): Which tracks interested in

**Display Previous Period:**
- `coach_career` (notes): Previous career planning notes

**User Enters (Current Period):**
- `coach_career` (notes): Career notes

**Form**: coach_rev

---

### Section 6: Goals & ILP Review

**Previous Period Goals** (from ilp form, previous period):
Three competency pairs with goal achievement status:
1. **Patient Care/Medical Knowledge**:
   - `prior_goal_pcmk` (yesno): Did you reach previous goal?
   
2. **System Based Practice/Problem Based Learning**:
   - `prior_goal_sbppbl` (yesno): Did you reach previous goal?
   
3. **Professionalism/Interpersonal Communication Skills**:
   - `prior_goal_profics` (yesno): Did you reach previous goal?

**Review Questions** (if present for current period):
- `review_q_pcmk` (text): If did not reach goal for PC/MK
- `review_q2_pcmk` (text): If reached goal for PC/MK
- `review_q_sbppbl` (text): If did not reach goal for SBP/PBL
- `review_q2_sbppbl` (text): If reached goal for SBP/PBL
- `review_q_profics` (text): If did not reach goal for PROF/ICS
- `review_q2_profics` (text): If reached goal for PROF/ICS

**Current Period Goals** (from ilp form, current period):
For each of the three competency pairs:
1. **Patient Care/Medical Knowledge**:
   - `goal_pcmk` (dropdown): Selected milestone description/level
   - `how_pcmk` (notes): How plan to reach this level
   - Associated milestone desc fields (see Milestone section)

2. **System Based Practice/Problem Based Learning**:
   - `goal_sbppbl` (dropdown): Selected milestone description/level
   - `how_sbppbl` (notes): How plan to reach this level
   - Associated milestone desc fields

3. **Professionalism/Interpersonal Communication Skills**:
   - `goal_profics` (dropdown): Selected milestone description/level
   - `how_profics` (notes): How plan to reach this level
   - Associated milestone desc fields

**Display Previous Period:**
- `coach_mile_goal` (notes): Previous coach notes on goals

**User Enters (Current Period):**
- `coach_mile_goal` (notes): Notes on goals

**Form**: coach_rev

---

### Section 7: ILP Summary

**User Enters (Current Period):**
- `coach_ilp_final` (notes): Finalized ILP summary
- **Format**: Copy exact format from previous imslu.coach.dash app
- This is the comprehensive ILP summary that synthesizes all sections

**Form**: coach_rev

---

### Section 8: Milestone Entry

**Spider Plot Visualization** (Using gmed module):
- `mod_milestone_spider_ui/server` OR `milestone_comparison_ui/server`
- **Compare**:
  - Current self-assessment: `milestone_selfevaluation_c33c` form (rep_*_self fields)
  - Last period ACGME milestones: `acgme_miles` form (acgme_* fields)

**Milestone Description Table** (Above milestone entry form):
Display all milestone *_desc fields with entries from **current period only**:
- From `milestone_selfevaluation_c33c` form
- Fields ending in `_self_desc`:
  - `rep_pc1_self_desc`, `rep_pc2_self_desc`, `rep_pc3_self_desc`, `rep_pc4_self_desc`, `rep_pc5_self_desc`, `rep_pc6_self_desc`
  - `rep_mk1_self_desc`, `rep_mk2_self_desc`, `rep_mk3_self_desc`
  - `rep_sbp1_self_desc`, `rep_sbp2_self_desc`, `rep_sbp3_self_desc`
  - `rep_pbl1_self_desc`, `rep_pbl2_self_desc`
  - `rep_prof1_self_desc`, `rep_prof2_self_desc`, `rep_prof3_self_desc`, `rep_prof4_self_desc`
  - `rep_ics1_self_desc`, `rep_ics2_self_desc`, `rep_ics3_self_desc`

**Table Columns**:
1. Milestone Code (e.g., PC1, SBP3, PROF2)
2. Self-Assessment Score (numeric from milestone_selfevaluation_c33c)
3. Description Text (from *_desc field)

**Milestone Entry Form**:
Pre-populate with values from `milestone_selfevaluation_c33c` (rep_*_self fields)

**Milestone Codes/Fields**:
- **Patient Care**: rep_pc1, rep_pc2, rep_pc3, rep_pc4, rep_pc5, rep_pc6
- **Medical Knowledge**: rep_mk1, rep_mk2, rep_mk3
- **Systems-Based Practice**: rep_sbp1, rep_sbp2, rep_sbp3
- **Practice-Based Learning**: rep_pbl1, rep_pbl2
- **Professionalism**: rep_prof1, rep_prof2, rep_prof3, rep_prof4
- **Interpersonal Communication**: rep_ics1, rep_ics2, rep_ics3

**Submit To Form**: `milestone_entry`
- Uses same field names: rep_pc1, rep_pc2, etc. (without _self suffix)
- Also includes rep_*_desc fields for coach descriptions

---

### Completion Checklist

Before enabling submit button, verify:
1. ☐ Section 1: Wellness notes entered
2. ☐ Section 2: Evaluations and plus/delta comments entered
3. ☐ Section 3: Board prep notes entered
4. ☐ Section 4: Scholarship notes entered
5. ☐ Section 5: Career notes entered
6. ☐ Section 6: Goals notes entered
7. ☐ Section 7: ILP final summary entered
8. ☐ Section 8: Milestones reviewed and entered

**Submit Button**: Enabled only when all required sections complete

---

## Submission Details

### Form: coach_rev (Primary Coaching Review)

**Metadata Fields**:
- `coach_date`: Auto-filled with current date (YYYY-MM-DD)
- `coach_period`: Auto-filled with selected period
- `coach_rev_complete`: "2" when complete, "0" when incomplete

**Content Fields** (from each section):
1. `coach_wellness` (notes)
2. `coach_evaluations` (notes)
3. `coach_p_d_comments` (notes)
4. `coach_step_board` (notes)
5. `coach_scholarship` (notes)
6. `coach_career` (notes)
7. `coach_mile_goal` (notes)
8. `coach_ilp_final` (notes)

**Additional Fields** (if present in form):
- `coach_pre_rev` (notes): Pre-review discussion points
- `coach_intro_back` (notes): Intern intro background (if intro period)
- `coach_coping` (notes): How dealing with residency
- `coach_ls_and_topic` (notes): Learning styles and topics
- `coach_summary` (notes): Overall summary
- `coach_anyelse` (notes): Anything else to discuss

### Form: second_review (Secondary Review)

**Fields**:
- `second_date` (text): Date of review
- `second_period` (dropdown): Period reviewed
- `second_approve` (yesno): Agree with milestones as reported
- `second_comments` (notes): Comments on ILP or anything else
- `second_miles_comment` (notes): Comments on changes to milestones

### Form: milestone_entry (Milestone Submission)

**Metadata**:
- `prog_mile_date`: Date of entry
- `prog_mile_period`: Milestone period

**Milestone Scores** (slider fields, 0-5):
All rep_* fields (19 total milestone ratings)

**Milestone Descriptions** (notes fields):
All rep_*_desc fields (optional coach descriptions for each milestone)

---

## Data Refresh Strategy

### After Completion of Review
1. Reload data for current period using `gmed::load_rdm_complete()`
2. Update resident selection table
3. Refresh completion status indicators
4. Clear form inputs
5. Return to resident selection table
6. Show success message with link to completed review

### On Period Change
1. Reload ALL data for selected period
2. Reload data for previous period (or NA if no previous)
3. Update all section displays
4. Clear any draft entries
5. Re-calculate completion status

---

## File Structure

```
imslu.coach.dash/
├── app.R                           # Main app entry point
├── manifest.json                   # Posit Cloud Connect manifest
├── R/
│   ├── globals.R                   # Config, constants, data loading
│   ├── ui.R                        # Main UI layout
│   ├── server.R                    # Main server logic
│   ├── config/
│   │   └── period_config.R         # Period definitions and logic
│   ├── modules/
│   │   ├── mod_login.R             # Authentication module
│   │   ├── mod_coach_select.R      # Coach selection module
│   │   ├── mod_resident_table.R    # Resident selection table module
│   │   └── wrappers/
│   │       ├── mod_coach_assessment_wrapper.R    # Assessment section wrapper
│   │       ├── mod_coach_milestone_wrapper.R     # Milestone section wrapper
│   │       ├── mod_coach_scholarship_wrapper.R   # Scholarship section wrapper
│   │       └── mod_coach_review_form.R           # Main accordion review form
│   └── utils/
│       ├── submission_helpers.R    # REDCap submission functions
│       ├── data_helpers.R          # Data filtering and processing
│       └── completion_helpers.R    # Completion status checking
├── www/
│   ├── styles.css                  # Custom styles
│   └── app.js                      # Client-side JavaScript
└── README.md                       # Documentation
```

---

## Wrapper Module Pattern

Following self-assessment app architecture:

### Purpose
Wrappers separate concerns:
- **gmed package**: Pure visualization and data processing
- **App code**: Entry forms, workflow management, REDCap submissions

### Structure
Each wrapper module:
1. Filters data for selected resident and period
2. Calls appropriate gmed visualization modules
3. Manages period-specific display logic
4. Handles form submissions to REDCap
5. Returns validation status

### Example: Assessment Wrapper
```r
# mod_coach_assessment_wrapper.R
mod_coach_assessment_wrapper_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Previous period data display
    uiOutput(ns("prev_period_display")),
    
    # gmed visualization modules
    gmed::assessment_viz_ui(ns("assessment_viz")),
    gmed::mod_plus_delta_table_ui(ns("plus_delta")),
    
    # Current period data display
    uiOutput(ns("current_period_display")),
    
    # Entry form
    textAreaInput(ns("coach_evaluations"), ...),
    textAreaInput(ns("coach_p_d_comments"), ...)
  )
}

mod_coach_assessment_wrapper_server <- function(id, app_data, selected_resident, current_period, previous_period) {
  moduleServer(id, function(input, output, session) {
    # Filter data for this resident
    resident_data <- reactive({
      req(app_data(), selected_resident())
      filter_resident_data(app_data(), selected_resident(), current_period())
    })
    
    # Call gmed modules
    gmed::assessment_viz_server("assessment_viz", resident_data)
    gmed::mod_plus_delta_table_server("plus_delta", resident_data)
    
    # Render displays
    output$prev_period_display <- renderUI({...})
    output$current_period_display <- renderUI({...})
    
    # Return entered data for submission
    return(reactive({
      list(
        coach_evaluations = input$coach_evaluations,
        coach_p_d_comments = input$coach_p_d_comments
      )
    }))
  })
}
```

---

## Critical Implementation Notes

### 1. Checkbox Field Handling
**CRITICAL**: Must use `raw_or_label = "raw"` in `load_rdm_complete()` because checkbox fields return numeric codes in raw format (e.g., "1,3,5" for checked options) which must be parsed correctly.

### 2. Period Instance Calculation
Use `gmed::get_redcap_instance(level, period, review_type = "scheduled")` to determine correct repeating instance number for submissions.

### 3. Completion Status Logic
Check multiple indicators:
- Form completion status fields (*_complete = "2")
- Presence of substantive data in key fields
- Most recent instance data for repeating forms

### 4. Second Review Access
If coach is listed in `second_rev` field:
- Show second_review completion status in table
- Pull up second_review data when selected (implementation TBD)
- Submit to second_review form instead of coach_rev form

### 5. Milestone Description Display
**Only show *_desc fields from current period** that have actual text entries (not blank/NA)
Table should dynamically filter to only milestones with descriptions entered

### 6. ITE Display Logic
Show ITE scores based on resident's PGY year at time of period being reviewed
Use historical data if reviewing past periods

### 7. Risk Status Calculation
Derive from most recent ccc_review entry:
- Check `ccc_concern` = "Yes"
- Display any active `ccc_action` items
- Show follow-up status from `ccc_action_status`

---

## Open Questions / TBD

1. **Ad Hoc Review Button**: Implementation details for separate ad hoc review workflow
2. **Second Review Data Pull**: Exact logic for when/how to load second_review data vs coach_rev data
3. **Risk Status Display**: Exact format/styling for risk indicator
4. **Historical ITE Data**: How to display ITE trends across years
5. **Milestone Spider Plot**: Which gmed function to use (mod_milestone_spider vs milestone_comparison)

---

## Testing Checklist

Before deployment:
- [ ] Data loading with raw format works correctly
- [ ] Period selection updates all sections
- [ ] Previous period data displays correctly
- [ ] Completion status indicators accurate
- [ ] All gmed modules render properly
- [ ] Form submissions include all required fields
- [ ] Instance calculation works for repeating forms
- [ ] Checkbox fields parse correctly
- [ ] Milestone descriptions filter to current period only
- [ ] Second review access works when coach is second reviewer
- [ ] Ad hoc review button disabled/hidden as expected

---

## Dependencies

### R Packages
- shiny
- shinydashboard or bslib (for UI framework)
- gmed (fbuckhold3/gmed on GitHub)
- REDCapR or httr (for API submissions)
- dplyr, tidyr (data manipulation)
- ggplot2, plotly (for any custom viz)

### External
- REDCap API access (token in environment variable or secure config)
- Posit Cloud Connect for deployment

---

*Document Version: 1.0*
*Last Updated: 2024-11-24*
*Based on: rdm2_data_dict_10_22_25.csv (520 rows)*
