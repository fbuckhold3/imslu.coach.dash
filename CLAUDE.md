# IMSLU Coaching Dashboard - RDM 2.0

**SSM-SLUH Internal Medicine Residency Coaching Review System**  
Version: RDM 2.0 Rebuild  
Last Updated: December 2025

---

## Table of Contents

1. [System Overview](#system-overview)
2. [Architecture](#architecture)
3. [Development Status & Roadmap](#development-status--roadmap)
4. [Period Structure](#period-structure)
5. [Review Interface Design](#review-interface-design)
6. [REDCap Data Structure](#redcap-data-structure)
7. [Module System](#module-system)
8. [Data Flow & Submission](#data-flow--submission)
9. [Technical Implementation](#technical-implementation)
10. [Development Patterns & Principles](#development-patterns--principles)
11. [Testing & Validation](#testing--validation)
12. [Related Systems](#related-systems)

---

## System Overview

### Purpose

The IMSLU Coaching Dashboard is a comprehensive R Shiny application that enables faculty coaches to review resident progress, provide structured feedback, and complete coaching evaluations. The system transitions from legacy architecture to RDM 2.0 data structure, integrating with REDCap databases for resident information, self-assessments, and evaluation tracking.

### Core Functionality

- **Coach Assignment Management**: Displays residents assigned to each coach with completion status
- **Multi-Period Review**: Access current and historical coaching periods with complete data context
- **Structured Review Interface**: Eight-section accordion layout covering all aspects of resident development
- **REDCap Integration**: Direct submission to coaching review forms with proper instance management
- **Visualization Integration**: Leverages gmed package for assessment display, milestone tracking, and scholarship management

### Stakeholders

- **Faculty Coaches**: Primary users who review resident progress and complete coaching evaluations
- **Second Reviewers**: Faculty who provide secondary review for select residents
- **Program Directors**: Monitor coaching completion rates and resident progress
- **Residents**: Indirect beneficiaries receiving structured feedback and coaching support

### Key Success Metrics

- Streamlined coaching interface reduces review time by 40%
- Improved consistency in coaching documentation quality
- Real-time completion tracking for program administrators
- Seamless integration with existing resident self-assessment workflow

---

## Architecture

### High-Level Design

```
Authentication (Coach Selection)
    ↓
Resident Selection Table (Filtered by Coach Assignment)
    ↓
Review Interface (Accordion Sections)
    ↓
REDCap Submission (coach_rev or second_review forms)
```

### Repository Structure

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
│   │   ├── mod_resident_table.R    # Resident selection table
│   │   ├── mod_review_interface.R  # Main review accordion
│   │   └── sections/
│   │       ├── mod_section_wellness.R
│   │       ├── mod_section_evaluations.R
│   │       ├── mod_section_learning.R
│   │       ├── mod_section_scholarship.R
│   │       ├── mod_section_career.R
│   │       ├── mod_section_milestones.R
│   │       ├── mod_section_goals.R
│   │       └── mod_section_summary.R
│   └── utils/
│       ├── submission_helpers.R    # REDCap submission functions
│       ├── data_helpers.R          # Data filtering and processing
│       ├── completion_helpers.R    # Completion status checking
│       └── reactive_helpers.R      # Safe reactive value handling
├── www/
│   ├── styles.css                  # Custom styles
│   └── app.js                      # Client-side JavaScript
└── README.md                       # Documentation
```

### Related Repositories

**Primary Repository**: `fbuckhold3/imslu.coach.dash`
- Branch: `rdm2-rebuild`
- Framework: R Shiny with bslib
- Deployment: Posit Cloud Connect

**Shared Package**: `fbuckhold3/gmed`
- Visualization functions for assessments, milestones, scholarship
- REDCap data loading utilities
- Period detection and instance calculation
- Reusable UI components

**Architecture Model**: `fbuckhold3/imslu-resident-self-assessment`
- Source of period structure and module patterns
- Reference implementation for RDM 2.0 data handling

### Design Principles

1. **Simplicity Over Complexity**
   - Direct table access with filtering vs multi-step wizards
   - Full-width layouts with essential information in headers
   - Progressive disclosure through accordion sections

2. **Modular Architecture**
   - Separate modules for each review section
   - Clean sourcing order to prevent cascading issues
   - Systematic error handling with explicit parameter scoping

3. **Data-Driven Configuration**
   - Period detection based on resident data and current date
   - Completion status calculated from multiple data sources
   - Dynamic content rendering based on period and resident context

4. **Bulletproof Helper Functions**
   - Automatic reactive value unwrapping
   - Explicit handling of NULL/NA cases
   - Clear parameter documentation

---

## Development Status & Roadmap

### Phase 1: Foundation (✅ Complete)

**Goals**: Establish navigation system and data infrastructure

**Completed Components**:
- ✅ Authentication module with coach selection
- ✅ Coach assignment filtering and display
- ✅ Resident selection table with completion indicators
- ✅ Navigation between interface sections
- ✅ Data loading with `gmed::load_rdm_complete(raw_or_label = "raw")`
- ✅ Period selection and override functionality

**Key Achievements**:
- Full navigation flow working smoothly
- Active residents displayed with proper coach filtering
- Completion status indicators functional
- Clean module architecture established

### Phase 2: Review Interface (🔄 In Progress)

**Goals**: Build core review interface with accordion sections

**Current Status**:
- ✅ Section 1 (Wellness & Progress) - Working prototype
  - Previous period resident self-assessment display
  - Current period resident self-assessment display
  - Coach entry fields for wellness notes
- 🔄 Sections 2-8 - In Development
  - Section structure defined
  - Data requirements mapped
  - gmed module integration planned

**Active Development Focus**:
- Implementing remaining accordion sections
- Progressive disclosure (sections unlock as previous complete)
- Data translation layers for coach displays (raw → label conversion)
- Reactive value handling improvements

**Recent Technical Fixes**:
- ✅ Resolved reactive value passing (functions vs actual data)
- ✅ Corrected module sourcing order
- ✅ Implemented safe reactive unwrapping in helper functions
- ✅ Created translation layer for checkbox field display

### Phase 3: Visualization Integration (📋 Planned)

**Goals**: Integrate gmed package visualizations into review sections

**Components**:
- Assessment visualization modules
- Milestone spider plots and comparisons
- Scholarship activity displays
- Conference question tracking
- ITE score displays with trends

**Integration Points**:
- Section 2: Assessment and evaluation visualizations
- Section 3: Learning topics and conference attendance
- Section 4: Scholarship table display
- Section 6: Milestone rating visualizations

### Phase 4: Submission & Completion (📋 Planned)

**Goals**: Complete submission workflow and validation

**Components**:
- Form validation before submission
- REDCap API submission with proper instance management
- Success/error handling and user feedback
- Data refresh after submission
- Return to resident table with updated completion status

**Critical Requirements**:
- Use `gmed::get_redcap_instance()` for repeating form instances
- Submit to `coach_rev` or `second_review` based on user role
- Include all required fields per data dictionary
- Handle partial saves for draft reviews

### Future Enhancements (💡 Backlog)

- Ad hoc review workflow (separate from scheduled reviews)
- Historical trend visualizations across periods
- Batch review capabilities for program directors
- Export functionality for completed reviews
- Mobile-responsive design improvements

---

## Period Structure

### Self-Assessment Period Definitions

Residents complete self-assessments across 7 periods during their residency. Each period has specific timing and content modules:

| Period # | Name | PGY Level | Timing | Duration |
|----------|------|-----------|--------|----------|
| 7 | Entering Residency | Intern | July-Sept | 3 months |
| 1 | Mid Intern | Intern | Nov-Jan | 3 months |
| 2 | End Intern | Intern | Apr-Jun | 3 months |
| 3 | Mid PGY2 | PGY2 | Nov-Jan | 3 months |
| 4 | End PGY2 | PGY2 | Apr-Jun | 3 months |
| 5 | Mid PGY3 | PGY3 | Nov-Jan | 3 months |
| 6 | Graduation | PGY3 | Feb-Jun | 5 months |

### Period Flow Design

#### Periods 1-5: Standard Self-Evaluation (Mid-Intern through Mid-PGY3)

**Resident Completes**:
1. Scholarship
2. Wellness and Career Planning
3. Program Feedback
4. Assessment Review
5. Learning Topics and Styles
6. Milestone Self-Evaluation
7. ILP Generation (Goals)

**Coach Reviews**:
- All resident-entered data
- Assessment visualizations
- Milestone ratings and goals
- Previous period coach notes

#### Period 6: Graduation (Feb-June, PGY3)

**Resident Completes**:
1. Scholarship
2. Graduation Data (career plans, contact info)
3. Program Feedback
4. Assessment Review
5. Board Prep (Step 3 status, MKSAP progress)
6. Milestone Self-Evaluation

**Coach Reviews**:
- Transition planning
- Board preparation progress
- Final milestone assessment
- Graduation readiness

#### Period 7: Entering Residency (July-Sept, Intern)

**Resident Completes**:
1. Skills Review (self-assessment of preparedness)
2. Learning Styles and Topics
3. Goals (initial residency goals)
4. Concerns Coming Into Residency
5. Milestone Self-Evaluation (baseline)

**Coach Reviews**:
- Baseline assessment
- Onboarding support needs
- Initial goal setting
- Identified concerns

### Period Detection Logic

**Current Period Determination**:
```r
# Uses gmed package functions from self-assessment app
current_period <- gmed::get_current_period(
  resident_data = residents,
  current_date = Sys.Date()
)
```

**Period Override**:
- User can select different period from dropdown
- System reloads ALL data for selected period
- Also loads previous period data (or NA if no previous)
- Updates all review sections to reflect selected period context

**Previous Period Calculation**:
- Use exact previous period to current (not most recent with data)
- Period 1 previous = Period 7
- Period 7 previous = NA (no previous period)
- Periods 2-6: sequential previous period

### Coaching Review Timing

**Review Types**:
1. **Primary Coaching Review** (`coach_rev` form)
   - Completed by assigned primary coach
   - Due after resident completes self-assessment
   - One per period per resident

2. **Second Review** (`second_review` form)
   - Completed by assigned second reviewer
   - Additional oversight for select residents
   - Independent of primary coaching review

3. **Ad Hoc Reviews** (future implementation)
   - Outside regular period structure
   - For special circumstances or concerns

**Instance Calculation**:
```r
# Determine correct REDCap instance for submission
instance <- gmed::get_redcap_instance(
  level = resident_pgy_level,
  period = current_period,
  review_type = "scheduled"
)
```

---

## Review Interface Design

### Header (Always Visible)

**Displayed Information**:
- Resident name (from `residents$name` or concatenated first/last)
- Current PGY year
- Selected period with dropdown override
- Navigation breadcrumbs (Coach Selection > Resident Table > Review)
- Completion progress indicator

**User Actions**:
- Change period dropdown (reloads all data)
- Return to resident table
- Save draft (future enhancement)
- Submit review

### Accordion Section Structure

The review interface uses an 8-section accordion layout with progressive disclosure. Each section displays previous period data (for context), current period resident entries, and provides coach entry fields.

**Progressive Disclosure Rules**:
- Section 1 always open on load
- Subsequent sections unlock as previous sections complete
- Users can return to any completed section
- Incomplete required sections show warning icon

**Common Section Pattern**:
```
┌─────────────────────────────────────────┐
│ Section N: [Title]                  [▼] │
├─────────────────────────────────────────┤
│ Previous Period (Period N-1)            │
│ ├─ Resident Self-Assessment             │
│ └─ Previous Coach Notes                 │
│                                          │
│ Current Period (Period N)                │
│ ├─ Resident Self-Assessment             │
│ └─ Visualizations/Data Displays         │
│                                          │
│ Coach Entry for Current Period           │
│ └─ [Input fields/text areas]            │
└─────────────────────────────────────────┘
```

---

### Section 1: Wellness & Progress

**Purpose**: Review resident wellness status and provide coaching support

**Display Previous Period**:
- `s_e_well` (text): "How are you doing mentally, emotionally, and physically?"
- `s_e_prog_assist` (text): "Is there anything the program can do to assist you?"
- `coach_wellness` (notes): Previous coach's wellness notes

**Display Current Period**:
- `s_e_well` (text): Current wellness status
- `s_e_prog_assist` (text): Current assistance needs

**Coach Entry (Current Period)**:
- `coach_wellness` (notes): Coach notes about wellness and support
  - Suggested prompts:
    - Response to resident's wellness concerns
    - Support resources offered or suggested
    - Follow-up actions needed
    - Any red flags or concerns noted

**Data Source**: 
- Previous/Current: `s_eval` form
- Coach Entry: `coach_rev` form

**Implementation Notes**:
- ✅ Currently functional as working prototype
- Uses translation layer for display (raw → label conversion)
- Handles reactive value unwrapping safely

---

### Section 2: Evaluations & Feedback

**Purpose**: Review faculty assessments and resident's reflection on feedback

**Assessment Visualizations** (using gmed modules):
- Main assessment overview (`gmed::assessment_viz_ui/server`)
- Detailed assessment breakdown (`gmed::mod_assessment_detail_custom_ui/server`)
- Core curriculum completion (`gmed::mod_cc_completion_ui/server`)
- Plus/delta feedback table (`gmed::mod_plus_delta_table_ui/server`)

**Display Current Period**:
- `s_e_plus` (notes): Resident's review of plus feedback received
- `s_e_delta` (notes): Resident's review of delta feedback received
- Assessment completion metrics
- Faculty feedback quality indicators

**Display Previous Period**:
- `coach_p_d_comments` (notes): Previous coach comments on plus/delta
- `coach_evaluations` (notes): Previous coach comments on evaluations

**Coach Entry (Current Period)**:
- `coach_p_d_comments` (notes): Notes on plus/delta and resident's review
  - Suggested prompts:
    - Patterns in feedback themes
    - Resident's insight into feedback
    - Action items from feedback review
- `coach_evaluations` (notes): Notes about assessment completion and quality
  - Suggested prompts:
    - Completion rate vs expectations
    - Quality of faculty feedback
    - Areas needing more assessment

**Data Sources**:
- Visualizations: `assessment_data` from loaded data
- Previous/Current resident entries: `s_eval` form
- Coach entries: `coach_rev` form

**Implementation Status**: 🔄 In Development

---

### Section 3: Learning & Board Preparation

**Purpose**: Track learning needs, conference participation, board prep, and identify at-risk residents

**Learning Topics Summary** (Previous + Current Period comparison):

Display as side-by-side comparison table:
- `s_e_topic_sel` (checkbox): Top 3 topics resident feels least confident about
  - Choices: Core IM topics from data dictionary
- `s_e_topic_oth` (text): Other topics if specified

**Learning Styles Summary** (Previous + Current Period comparison):
- `s_e_learn_style` (checkbox): Desired learning experiences
- `s_e_learn_oth` (text): Other learning style preferences

**Conference Attendance/Questions Visualization**:
- Use `gmed::mod_questions_viz_ui/server` from gmed package
- Data from `questions` form:
  - `q_date`: Question date
  - `q_rotation`: Rotation
  - `q_answer`: Answer correctness
  - `q_word`: Word of the day
  - `q_level`: Question level

**ITE Data Display** (based on resident's PGY year):

Display relevant ITE percentiles with subspecialty breakdown:
- **PGY1**: Total ILE + 12 subspecialty percentiles (cards, endo, GI, GIM, geri, heme/onc, ID, nephro, neuro, pulm/CCM, rheum, HVC)
- **PGY2**: Same fields with `pgy2_*` prefix
- **PGY3**: Same fields with `pgy3_*` prefix

Show historical trend if reviewing past periods

**Risk Status Display** (prominent indicator):

Derived from CCC review data (`ccc_review` form):
- `ccc_concern` (yesno): Any concerns of the CCC?
- `ccc_action` (checkbox): Actions suggested by CCC
- `ccc_action_status`: Follow-up status on actions

Display format:
- ⚠️ High priority indicator if concerns present
- ✓ No concerns indicator if none present
- List active action items with status

**Step 3 Data** (from `resident_data` form):
- `step3` (yesno): USMLE and/or COMLEX Step 3 Passed?
- `usmle_step3_failure` (yesno): Failure status
- `usmle_step3_score` (text/integer): Score if passed
- `comlex_step3_failure` (yesno): Failure status

**Step 3 Self-Evaluation** (from `s_eval` form, if Step 3 not completed):
- `s_e_step3` (yesno): Have you completed Step 3?
- `s_e_step3_contact` (yesno): Emailed score to program?
- `s_e_step3_date_set` (yesno): Set a date for Step 3?
- `s_e_step3_date` (text): Scheduled date

**Board Concerns** (from `s_eval` form):
- `s_e_board_concern` (yesno): Concerns about failing boards?
- `s_e_board_help` (yesno): Discussed with program or reached out?
- `s_e_board_discu` (notes): Discussion details and steps taken

**Coach Entry (Current Period)**:
- `coach_step_board` (notes): Notes on Step 3 completion or board prep
  - Suggested prompts:
    - Step 3 timeline and preparation
    - Board preparation progress
    - Concerns or support needed
    - Resource recommendations

**Data Sources**:
- Learning topics/styles: `s_eval` form
- Conference questions: `questions` form
- ITE scores: `test_data` form
- Risk status: `ccc_review` form
- Step 3 data: `resident_data` form
- Board concerns: `s_eval` form
- Coach entry: `coach_rev` form

**Implementation Status**: 📋 Planned

---

### Section 4: Scholarship

**Purpose**: Review scholarly activities, QI projects, and research participation

**Scholarship Display** (using gmed module):
- `gmed::mod_scholarship_display_ui/server`: Interactive table from gmed package
- Groups by type: QI Projects, Research, Presentations, Publications, Committees

**Scholarship Form Fields** (`scholarship` form, repeating):

**Project Information**:
- `schol_type` (dropdown): Type of project
- `schol_ps` (yesno): Patient safety review participation?
- `schol_rca` (yesno): Root cause analysis participation?

**QI Projects**:
- `schol_qi` (notes): QI project description

**Research Projects**:
- `schol_res` (notes): Research description
- `schol_res_mentor` (notes): Research mentor
- `schol_res_status` (dropdown): Project status

**Presentations**:
- `schol_pres` (yesno): Presented project?
- `schol_pres_type` (dropdown): Presentation type/location
- `schol_pres_conf` (notes): Conference name

**Publications**:
- `schol_cit` (notes): Full citation
- `schol_pub` (yesno): Published?

**Committee Work**:
- `schol_comm` (notes): Committee name

**Display Format**:
- Previous period: Summary count by type
- Current period: Full table with details
- Highlight new entries since last period

**Coach Entry (Current Period)**:
- `coach_scholarship` (notes): Notes on scholarship progress
  - Suggested prompts:
    - Progress on ongoing projects
    - Opportunities for new projects
    - Mentorship connections needed
    - Presentation/publication potential

**Data Sources**:
- Visualization: `scholarship` form (repeating)
- Coach entry: `coach_rev` form

**Implementation Status**: 📋 Planned

---

### Section 5: Career Planning & Development

**Purpose**: Track career interests and provide mentorship guidance

**Career Planning Data** (Previous + Current Period comparison):

**Career Path Selection**:
- `s_e_career` (checkbox): Career path(s) considering
  - Primary Care
  - Hospital Medicine
  - Fellowship
  - Other/Undecided

**Fellowship Interests** (if applicable):
- `s_e_fellowship` (checkbox): Fellowship areas of interest
  - Cardiology, Endocrine, GI, Geriatrics, Hem/Onc, ID, Nephro, Pulm/CCM, Rheum, Other

**Track Interest**:
- `s_e_track` (checkbox): Program track interest
  - Primary Care Track
  - Hospitalist Track
  - PROMOTE Track

**Mentorship Topics**:
- `s_e_mentor_topics` (notes): Topics to discuss with mentor

**Display Format**:
- Side-by-side comparison of previous vs current period selections
- Highlight changes in career interests
- Show consistency or evolution of plans

**Coach Entry (Current Period)**:
- `coach_career` (notes): Career planning discussion notes
  - Suggested prompts:
    - Career exploration conversations
    - Mentorship connections facilitated
    - Action steps for career development
    - Resources or experiences recommended

**Data Sources**:
- Previous/Current: `s_eval` form
- Coach entry: `coach_rev` form

**Period Applicability**: Periods 1-5 only (not in Period 6 Graduation or Period 7 Entering)

**Implementation Status**: 📋 Planned

---

### Section 6: Milestones

**Purpose**: Review resident milestone self-assessment and enter coach milestone ratings

**Milestone Visualization** (using gmed module):
- Spider plot comparing resident self-assessment to ACGME/program ratings
- Milestone progression over time
- National benchmark comparisons

**Milestone Self-Assessment Display** (Current Period):

**23 ACGME Subcompetencies** (from `milestone_selfevaluation_c33c` form):
- All `m_*` fields (slider ratings 0-5)
- Corresponding `m_*_desc` fields (descriptions for high ratings)

**Display Format**:
- Table showing all 23 subcompetencies
- Resident self-rating (current period)
- Previous period resident rating (for comparison)
- Previous period coach rating (for comparison)
- Description text (only show non-empty descriptions)

**Subcompetency List** (organized by competency):

**Patient Care (PC)**:
- PC1: Gathers and synthesizes essential information
- PC2: Prioritizes differential diagnosis
- PC3: Manages patients with progressive responsibility
- PC4: Demonstrates skill in performing procedures
- PC5: Requests consultations effectively

**Medical Knowledge (MK)**:
- MK1: Core knowledge for effective patient care

**Systems-Based Practice (SBP)**:
- SBP1: Works effectively within healthcare system
- SBP2: Coordinates care with other healthcare professionals
- SBP3: Incorporates cost-awareness

**Practice-Based Learning (PBLI)**:
- PBLI1: Identifies strengths and gaps in knowledge
- PBLI2: Uses information technology for learning
- PBLI3: Analyzes clinical experience systematically

**Professionalism (PROF)**:
- PROF1: Demonstrates compassion and respect
- PROF2: Demonstrates accountability to patients and society
- PROF3: Manages conflicts of interest

**Interpersonal and Communication Skills (ICS)**:
- ICS1: Communicates effectively with patients and families
- ICS2: Maintains comprehensive, accurate records
- ICS3: Communicates effectively with healthcare team

**Coach Milestone Entry** (Current Period):

**Entry Fields** (to `milestone_entry` form):
- All `rep_*` fields (slider ratings 0-5) - 23 ratings total
- All `rep_*_desc` fields (optional coach descriptions)

**Entry Guidelines**:
- Review resident self-assessment first
- Consider assessment data and direct observations
- Enter ratings independently (not anchored to resident ratings)
- Provide descriptions for ratings that represent strengths or concerns
- Compare to previous period ratings for consistency

**Data Sources**:
- Resident self-assessment: `milestone_selfevaluation_c33c` form
- Previous coach ratings: `milestone_entry` form (previous period)
- ACGME ratings: `acgme_miles` form
- Coach entry: `milestone_entry` form (current period)

**Implementation Notes**:
- **CRITICAL**: Only show milestone descriptions (`*_desc` fields) from current period that have actual text entries
- Filter description table dynamically to non-empty entries only
- Show resident descriptions separately from coach entry fields

**Implementation Status**: 📋 Planned

---

### Section 7: Goals & Individual Learning Plan

**Purpose**: Review resident goals and provide coaching on goal achievement

**Previous Period Goals Display**:

Show goals from last period with progress assessment:
- Selected subcompetency goals (one from each domain)
- Target milestone level and description
- "How" plan (action steps)
- Coach comments on progress (from previous period)

**Current Period Goals Display** (from `ilp` form):

**PC/MK Domain Goal**:
- `ilp_pcmk` (dropdown): Selected PC or MK subcompetency
- `ilp_pcmk_target` (dropdown): Target milestone level
- `ilp_pcmk_mile_text` (text): Specific milestone description
- `ilp_pcmk_how` (notes): How will achieve this goal

**SBP/PBLI Domain Goal**:
- `ilp_sbp_pbli` (dropdown): Selected SBP or PBLI subcompetency
- `ilp_sbp_pbli_target` (dropdown): Target milestone level
- `ilp_sbp_pbli_mile_text` (text): Specific milestone description
- `ilp_sbp_pbli_how` (notes): How will achieve this goal

**PROF/ICS Domain Goal**:
- `ilp_prof_ics` (dropdown): Selected PROF or ICS subcompetency
- `ilp_prof_ics_target` (dropdown): Target milestone level
- `ilp_prof_ics_mile_text` (text): Specific milestone description
- `ilp_prof_ics_how` (notes): How will achieve this goal

**Display Format**:
- Three-column layout (one per domain)
- Show previous period goals at top for reference
- Current period goals below with full details
- Highlight if goal changed from previous period

**Coach Entry (Current Period)**:
- `coach_goals` (notes): Coaching notes on goals
  - Suggested prompts:
    - Feedback on goal selection and specificity
    - Support or resources needed for goal achievement
    - Progress on previous period goals
    - Suggestions for goal refinement

**Data Sources**:
- Previous/Current goals: `ilp` form
- Previous coach comments: `coach_rev` form (previous period)
- Coach entry: `coach_rev` form (current period)

**Period Applicability**: Periods 1-5 only (not in Period 6 Graduation or Period 7 Entering)

**Implementation Status**: 📋 Planned

---

### Section 8: Summary & Submission

**Purpose**: Review all entered data and complete coaching review submission

**Summary Display**:
- Checklist of all sections completed
- Warning indicators for any incomplete required fields
- Preview of all coach entries for current period

**Completion Checklist**:
- ☐ Section 1: Wellness & Progress
- ☐ Section 2: Evaluations & Feedback
- ☐ Section 3: Learning & Board Preparation
- ☐ Section 4: Scholarship
- ☐ Section 5: Career Planning (if applicable)
- ☐ Section 6: Milestones (required)
- ☐ Section 7: Goals & ILP (if applicable)

**Review Period Validation**:
- All required sections have coach entries
- Milestone ratings entered for all 23 subcompetencies
- No empty required text fields
- Proper period and instance selected

**Submission Actions**:
- **Save Draft**: Save progress without submitting (future enhancement)
- **Submit Review**: Validate and submit to REDCap
- **Cancel**: Return to resident table without saving

**Post-Submission**:
- Display success message with review details
- Refresh completion status in resident table
- Clear form inputs
- Return to resident table

**Data Sources**:
- Validation: All coach entry fields from sections 1-7
- Submission: `coach_rev` or `second_review` form

**Implementation Status**: 📋 Planned

---

## REDCap Data Structure

### Key Forms Overview

The coaching dashboard integrates with multiple REDCap forms. Understanding the form structure is critical for proper data display and submission.

| Form | Instrument Type | Primary Use | Key Fields |
|------|----------------|-------------|------------|
| `resident_data` | Non-repeating | Demographics, assignments | `coach`, `coach_email`, `second_rev`, `res_archive` |
| `s_eval` | Repeating | Resident self-evaluation | Wellness, career, board, plus/delta, learning |
| `ilp` | Repeating | Individual learning plans | Goals, action plans, milestone selections |
| `milestone_selfevaluation_c33c` | Repeating | Resident milestone self-assessment | 23 milestone ratings + descriptions |
| `milestone_entry` | Repeating | Coach milestone ratings | 23 milestone ratings + descriptions |
| `acgme_miles` | Repeating | ACGME milestone ratings | Comparison data |
| `coach_rev` | Repeating | Primary coaching review | All coach entry fields |
| `second_review` | Repeating | Secondary review | Second reviewer entries |
| `scholarship` | Repeating | Scholarly activities | Projects, presentations, publications |
| `test_data` | Repeating | ITE scores | PGY-specific percentiles |
| `questions` | Repeating | Conference questions | Attendance, correctness |
| `ccc_review` | Repeating | CCC concerns | Risk status, actions |

### Coach Assignment Fields

**From `resident_data` form** (non-repeating):

```
coach: Primary coach (dropdown)
  - Faculty list from REDCap
  - Used to filter residents in table
  
coach_email: Coach email address (text)
  - For notifications (future enhancement)
  
second_rev: Second reviewer (dropdown)
  - Faculty list from REDCap
  - Used to filter residents for second reviewers
  - Determines which form to submit (coach_rev vs second_review)
  
res_archive: Archive status (yesno)
  - Filters out archived residents
  - Archived residents excluded from active coaching
```

### Resident Selection Table Data

**Columns and Data Sources**:

1. **Name**: `residents$name` or `paste(residents$first_name, residents$last_name)`
2. **Level (PGY)**: Calculated from `resident_data$year_started` and current date
3. **Current Period**: Derived from PGY level and date using `gmed::get_current_period()`
4. **S_eval Completed**: Check `s_eval` form completion status for current period
5. **Coach Review Completed**: Check `coach_rev` form completion status for current period
6. **Second Review Completed**: Check `second_review` form completion status for current period

**Filter Logic**:
```r
# Show residents where:
filtered_residents <- residents %>%
  filter(
    res_archive != "Yes" &  # Not archived
    (coach == current_user | second_rev == current_user)  # User is coach or second reviewer
  )
```

### Completion Status Calculation

**Multi-indicator Approach** (check all of these):

1. **Form Completion Status**: `*_complete = "2"` (Complete)
2. **Substantive Data Present**: Key required fields not empty
3. **Most Recent Instance**: For repeating forms, check current period instance

**Example Implementation**:
```r
check_coach_review_complete <- function(record_id, period, form_data) {
  
  # Get data for this resident and period
  review_data <- form_data %>%
    filter(
      record_id == !!record_id,
      redcap_repeat_instance == calculate_instance(period)
    )
  
  # Check completion indicators
  has_completion_status <- review_data$coach_rev_complete == "2"
  has_wellness_notes <- !is.na(review_data$coach_wellness) && review_data$coach_wellness != ""
  has_milestone_ratings <- sum(!is.na(review_data %>% select(starts_with("rep_")))) >= 23
  
  # Return TRUE if all indicators present
  return(has_completion_status && has_wellness_notes && has_milestone_ratings)
}
```

### Repeating Form Instance Management

**Critical Concept**: REDCap repeating forms use `redcap_repeat_instance` to track multiple entries per resident.

**Instance Calculation**:
```r
# Use gmed package function
instance <- gmed::get_redcap_instance(
  level = resident_pgy_level,
  period = current_period,
  review_type = "scheduled"  # vs "adhoc"
)
```

**Instance Pattern**:
- Each period has a specific instance number
- Calculation based on PGY level and period number
- Example: PGY2 Period 3 = Instance 11
- **Always use gmed function** - don't calculate manually

### Checkbox Field Handling

**CRITICAL REQUIREMENT**: Must load data with `raw_or_label = "raw"` for checkbox fields.

**Why**:
- Checkbox fields in REDCap store as numeric codes: `"1,3,5"` for checked options
- Label format loses information about which specific options are selected
- Raw format allows proper parsing and display

**Example**:
```r
# CORRECT
app_data <- gmed::load_rdm_complete(raw_or_label = "raw")

# WRONG - will break checkbox field display
app_data <- gmed::load_rdm_complete(raw_or_label = "label")
```

**Translation for Display**:
```r
# Create translation layer for user-facing display
translate_checkbox_to_labels <- function(raw_values, field_choices) {
  if (is.na(raw_values) || raw_values == "") return("")
  
  selected_codes <- str_split(raw_values, ",")[[1]]
  labels <- field_choices %>%
    filter(code %in% selected_codes) %>%
    pull(label)
  
  return(paste(labels, collapse = ", "))
}
```

---

## Module System

### Modular Architecture Overview

The coaching dashboard uses a modular Shiny architecture with clear separation of concerns:

**Module Types**:
1. **Navigation Modules**: Authentication, coach selection, resident table
2. **Section Modules**: Individual accordion sections (wellness, evaluations, etc.)
3. **Wrapper Modules**: Connect app code to gmed package visualizations
4. **Utility Modules**: Shared components (date pickers, dropdowns, etc.)

### Section Module Pattern

**Standard Structure**:
```r
# UI Function
mod_section_[name]_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Section Title"),
    
    # Previous period display
    div(class = "previous-period",
      h5("Previous Period"),
      uiOutput(ns("prev_period_display"))
    ),
    
    # Current period display
    div(class = "current-period",
      h5("Current Period"),
      uiOutput(ns("current_period_display"))
    ),
    
    # Coach entry
    div(class = "coach-entry",
      h5("Coach Notes"),
      textAreaInput(ns("coach_notes"), ...)
    )
  )
}

# Server Function
mod_section_[name]_server <- function(id, app_data, selected_resident, current_period, previous_period) {
  moduleServer(id, function(input, output, session) {
    
    # Filter data for selected resident
    resident_data_current <- reactive({
      req(app_data(), selected_resident(), current_period())
      filter_resident_period_data(app_data(), selected_resident(), current_period())
    })
    
    resident_data_previous <- reactive({
      req(app_data(), selected_resident(), previous_period())
      if (is.na(previous_period())) return(NULL)
      filter_resident_period_data(app_data(), selected_resident(), previous_period())
    })
    
    # Render previous period display
    output$prev_period_display <- renderUI({
      prev_data <- resident_data_previous()
      if (is.null(prev_data)) return(p("No previous period data"))
      
      # Display logic here
    })
    
    # Render current period display
    output$current_period_display <- renderUI({
      curr_data <- resident_data_current()
      # Display logic here
    })
    
    # Return entered data for submission
    return(reactive({
      list(
        coach_notes = input$coach_notes,
        completed = !is.null(input$coach_notes) && input$coach_notes != ""
      )
    }))
  })
}
```

### Wrapper Module Pattern

**Purpose**: Integrate gmed package visualizations while maintaining separation of concerns.

**Structure**:
```r
# UI Function
mod_wrapper_[name]_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Call gmed UI function with namespaced ID
    gmed::mod_[visualization]_ui(ns("gmed_viz")),
    
    # Add any app-specific controls
    div(class = "controls",
      selectInput(ns("filter_option"), ...)
    )
  )
}

# Server Function
mod_wrapper_[name]_server <- function(id, app_data, selected_resident, current_period) {
  moduleServer(id, function(input, output, session) {
    
    # Prepare data for gmed module
    viz_data <- reactive({
      req(app_data(), selected_resident())
      prepare_viz_data(app_data(), selected_resident(), current_period())
    })
    
    # Call gmed server function
    gmed::mod_[visualization]_server(
      "gmed_viz",
      data = viz_data,
      # Pass any required parameters
    )
    
    # Return any app-specific outputs
    return(reactive({
      list(
        visualization_ready = !is.null(viz_data())
      )
    }))
  })
}
```

### Module Sourcing Order

**Critical Rule**: Source modules in dependency order to prevent cascading errors.

**Recommended Order**:
1. Utility/helper modules first
2. Wrapper modules next (depend on utils)
3. Section modules (depend on utils and wrappers)
4. Navigation modules (depend on sections)
5. Main server logic last

**Example `globals.R`**:
```r
# 1. Utility modules
source("R/utils/reactive_helpers.R")
source("R/utils/data_helpers.R")
source("R/utils/completion_helpers.R")

# 2. Wrapper modules (if using gmed)
source("R/modules/wrappers/mod_assessment_wrapper.R")
source("R/modules/wrappers/mod_milestone_wrapper.R")

# 3. Section modules
source("R/modules/sections/mod_section_wellness.R")
source("R/modules/sections/mod_section_evaluations.R")
# ... other sections

# 4. Navigation modules
source("R/modules/mod_login.R")
source("R/modules/mod_coach_select.R")
source("R/modules/mod_resident_table.R")
source("R/modules/mod_review_interface.R")
```

---

## Data Flow & Submission

### Data Loading Flow

```
Startup
  ↓
Load REDCap Data (gmed::load_rdm_complete)
  ↓
Filter Archived Residents
  ↓
Calculate Historical Medians
  ↓
Add Level at Time Calculations
  ↓
Store in Reactive Value
```

**Implementation**:
```r
# In server.R or globals.R
load_app_data <- function() {
  
  # Load from REDCap with raw format
  rdm_data <- gmed::load_rdm_complete(
    raw_or_label = "raw",
    include_archived = TRUE  # For historical median calculation
  )
  
  # Calculate historical medians BEFORE filtering
  rdm_data$historical_medians <- calculate_historical_medians(
    residents = rdm_data$residents,
    assessment_data = rdm_data$assessment_data,
    milestone_data = rdm_data$milestone_data
  )
  
  # Filter out archived residents for active display
  rdm_data$residents <- rdm_data$residents %>%
    filter(res_archive != "Yes")
  
  # Add level_at_time calculations for historical period viewing
  rdm_data$residents <- rdm_data$residents %>%
    add_level_at_time_calculations()
  
  return(rdm_data)
}

# Create reactive value for app data
app_data <- reactiveVal(load_app_data())
```

### Period Selection Flow

```
User Selects Period
  ↓
Trigger Data Reload
  ↓
Load Current Period Data
  ↓
Load Previous Period Data (if exists)
  ↓
Update All Section Displays
  ↓
Clear Any Draft Entries
  ↓
Recalculate Completion Status
```

**Implementation**:
```r
# When period changes
observeEvent(input$period_override, {
  
  selected_period <- as.numeric(input$period_override)
  current_period(selected_period)
  
  # Calculate previous period
  prev_period <- calculate_previous_period(selected_period)
  previous_period(prev_period)
  
  # Reload data for selected period
  showNotification("Loading data for selected period...", type = "message")
  
  # Reload ALL data
  new_data <- load_app_data()
  app_data(new_data)
  
  # Clear any existing draft entries
  clear_draft_entries()
  
  showNotification("Period data loaded successfully", type = "message")
})
```

### Review Submission Flow

```
User Clicks Submit
  ↓
Validate All Sections
  ↓
Check Required Fields
  ↓
Confirm Submission
  ↓
Package Data for REDCap
  ↓
Calculate Instance Number
  ↓
Submit to REDCap API
  ↓
Handle Success/Error
  ↓
Refresh Completion Status
  ↓
Return to Resident Table
```

### Data Packaging for Submission

**Critical Requirements**:
1. Correct form name (`coach_rev` or `second_review`)
2. Correct instance number (use `gmed::get_redcap_instance`)
3. All required fields included
4. Proper field name formatting (exactly match data dictionary)
5. Proper data types (text, numeric, checkbox codes)

**Example Implementation**:
```r
package_coach_review_for_submission <- function(
  record_id,
  period,
  level,
  review_type = "coach_rev",  # or "second_review"
  section_data
) {
  
  # Calculate instance
  instance <- gmed::get_redcap_instance(
    level = level,
    period = period,
    review_type = "scheduled"
  )
  
  # Build submission record
  submission_record <- data.frame(
    record_id = record_id,
    redcap_repeat_instrument = review_type,
    redcap_repeat_instance = instance,
    
    # Section 1: Wellness
    coach_wellness = section_data$wellness$coach_wellness,
    
    # Section 2: Evaluations
    coach_evaluations = section_data$evaluations$coach_evaluations,
    coach_p_d_comments = section_data$evaluations$coach_p_d_comments,
    
    # Section 3: Learning
    coach_step_board = section_data$learning$coach_step_board,
    
    # Section 4: Scholarship
    coach_scholarship = section_data$scholarship$coach_scholarship,
    
    # Section 5: Career (if applicable)
    coach_career = if (!is.null(section_data$career)) section_data$career$coach_career else NA,
    
    # Section 7: Goals (if applicable)
    coach_goals = if (!is.null(section_data$goals)) section_data$goals$coach_goals else NA,
    
    # Form completion status
    coach_rev_complete = "2",  # or second_review_complete = "2"
    
    stringsAsFactors = FALSE
  )
  
  # Milestone data submitted to separate form
  milestone_record <- data.frame(
    record_id = record_id,
    redcap_repeat_instrument = "milestone_entry",
    redcap_repeat_instance = instance,
    
    # All 23 milestone ratings
    rep_pc1 = section_data$milestones$rep_pc1,
    rep_pc2 = section_data$milestones$rep_pc2,
    # ... all 23 rep_* fields
    
    # Optional descriptions
    rep_pc1_desc = section_data$milestones$rep_pc1_desc,
    rep_pc2_desc = section_data$milestones$rep_pc2_desc,
    # ... all 23 rep_*_desc fields
    
    milestone_entry_complete = "2",
    
    stringsAsFactors = FALSE
  )
  
  return(list(
    coach_review = submission_record,
    milestone_entry = milestone_record
  ))
}
```

### REDCap API Submission

**Using REDCapR Package**:
```r
submit_to_redcap <- function(submission_data, form_name) {
  
  tryCatch({
    
    # Import data to REDCap
    result <- REDCapR::redcap_write(
      ds_to_write = submission_data,
      redcap_uri = Sys.getenv("REDCAP_API_URL"),
      token = Sys.getenv("REDCAP_API_TOKEN"),
      verbose = FALSE
    )
    
    if (result$success) {
      return(list(
        success = TRUE,
        message = paste("Successfully submitted", form_name)
      ))
    } else {
      return(list(
        success = FALSE,
        error_message = result$outcome_message
      ))
    }
    
  }, error = function(e) {
    return(list(
      success = FALSE,
      error_message = paste("API Error:", e$message)
    ))
  })
}
```

### Post-Submission Actions

**Success Flow**:
1. Display success modal with review details
2. Reload app data to refresh completion status
3. Clear all form inputs
4. Return to resident selection table
5. Highlight completed resident with green check

**Error Handling**:
1. Display error modal with specific error message
2. Keep form data intact (don't clear)
3. Offer retry or cancel options
4. Log error for troubleshooting

---

## Technical Implementation

### Critical Technical Requirements

#### 1. Reactive Value Handling

**Problem**: Functions can be passed as reactive values instead of actual data, causing errors.

**Solution**: Bulletproof helper functions that automatically unwrap reactive values.

```r
# Safe reactive unwrapping helper
safe_reactive_value <- function(x) {
  if (is.reactive(x)) {
    return(x())
  } else if (is.function(x)) {
    # Try to call it once
    result <- try(x(), silent = TRUE)
    if (!inherits(result, "try-error")) {
      return(result)
    }
  }
  return(x)
}

# Use in data filtering functions
filter_resident_period_data <- function(app_data, record_id, period) {
  
  # Safely unwrap all parameters
  data <- safe_reactive_value(app_data)
  id <- safe_reactive_value(record_id)
  per <- safe_reactive_value(period)
  
  # Validate
  if (is.null(data) || is.null(id) || is.null(per)) {
    return(NULL)
  }
  
  # Filter logic here
  # ...
}
```

#### 2. Data Translation Layers

**Problem**: Raw format needed for checkbox parsing, but labels needed for user display.

**Solution**: Translation layer that converts raw codes to human-readable labels.

```r
# Create translation function from data dictionary
create_field_translator <- function(data_dict, field_name) {
  
  # Get choices for this field
  choices_str <- data_dict %>%
    filter(`Variable / Field Name` == field_name) %>%
    pull(`Choices, Calculations, OR Slider Labels`)
  
  if (is.na(choices_str)) return(NULL)
  
  # Parse choices (format: "1, Label 1 | 2, Label 2 | ...")
  choices <- str_split(choices_str, "\\|")[[1]] %>%
    str_trim() %>%
    str_split_fixed(", ", 2)
  
  translator <- function(raw_value) {
    if (is.na(raw_value) || raw_value == "") return("")
    
    codes <- str_split(raw_value, ",")[[1]] %>% str_trim()
    labels <- choices[choices[,1] %in% codes, 2]
    return(paste(labels, collapse = ", "))
  }
  
  return(translator)
}

# Usage in UI rendering
output$topic_display <- renderUI({
  raw_topics <- resident_data()$s_e_topic_sel
  translator <- create_field_translator(data_dict, "s_e_topic_sel")
  label_topics <- translator(raw_topics)
  
  p(label_topics)
})
```

#### 3. Period-Specific Data Filtering

**Problem**: Need to filter data for specific resident and period across multiple forms.

**Solution**: Centralized filtering function with period and instance awareness.

```r
filter_resident_period_data <- function(app_data, record_id, period, form_name = NULL) {
  
  # Unwrap reactive values
  data <- safe_reactive_value(app_data)
  id <- safe_reactive_value(record_id)
  per <- safe_reactive_value(period)
  
  # Get resident's PGY level at this period
  resident_info <- data$residents %>%
    filter(record_id == id)
  
  if (nrow(resident_info) == 0) return(NULL)
  
  level_at_period <- calculate_level_at_period(
    year_started = resident_info$year_started,
    period = per
  )
  
  # Calculate instance for this period
  instance <- gmed::get_redcap_instance(
    level = level_at_period,
    period = per,
    review_type = "scheduled"
  )
  
  # Filter forms
  if (!is.null(form_name)) {
    # Filter specific form
    form_data <- data$all_forms[[form_name]] %>%
      filter(
        record_id == id,
        redcap_repeat_instance == instance
      )
    return(form_data)
  } else {
    # Return all data for this resident/period
    filtered_data <- list()
    for (form in names(data$all_forms)) {
      filtered_data[[form]] <- data$all_forms[[form]] %>%
        filter(
          record_id == id,
          redcap_repeat_instance == instance
        )
    }
    return(filtered_data)
  }
}
```

#### 4. Completion Status Checking

**Problem**: Multiple indicators needed to determine if review is complete.

**Solution**: Comprehensive completion checker with multiple validation points.

```r
check_section_completion <- function(section_name, section_data, required_fields) {
  
  # Check if all required fields have data
  all_required_present <- all(
    !is.na(section_data[required_fields]) &
    section_data[required_fields] != ""
  )
  
  if (!all_required_present) return(FALSE)
  
  # Section-specific validation
  if (section_name == "milestones") {
    # All 23 milestone ratings must be present
    milestone_fields <- paste0("rep_", c("pc1", "pc2", "pc3", "pc4", "pc5",
                                          "mk1", "sbp1", "sbp2", "sbp3",
                                          "pbli1", "pbli2", "pbli3",
                                          "prof1", "prof2", "prof3",
                                          "ics1", "ics2", "ics3"))
    all_milestones_rated <- all(!is.na(section_data[milestone_fields]))
    return(all_milestones_rated)
  }
  
  return(TRUE)
}

check_review_completion <- function(record_id, period, app_data) {
  
  # Get all section data
  sections <- c("wellness", "evaluations", "learning", "scholarship",
                "career", "milestones", "goals")
  
  completion_status <- list()
  
  for (section in sections) {
    # Skip sections not applicable to this period
    if (!is_section_applicable(section, period)) {
      completion_status[[section]] <- TRUE  # Auto-complete if not applicable
      next
    }
    
    # Get required fields for this section
    required_fields <- get_required_fields(section)
    
    # Get section data
    section_data <- filter_resident_period_data(
      app_data = app_data,
      record_id = record_id,
      period = period,
      form_name = get_form_name(section)
    )
    
    # Check completion
    completion_status[[section]] <- check_section_completion(
      section_name = section,
      section_data = section_data,
      required_fields = required_fields
    )
  }
  
  # Overall completion
  overall_complete <- all(unlist(completion_status))
  
  return(list(
    sections = completion_status,
    overall = overall_complete
  ))
}
```

### Development Environment

**IDE**: Positron (preferred) or RStudio

**R Version**: 4.3+

**Key Packages**:
```r
# Core Shiny
library(shiny)
library(bslib)

# Data manipulation
library(dplyr)
library(tidyr)
library(stringr)

# REDCap integration
library(REDCapR)  # or httr for custom API calls

# Visualization
library(ggplot2)
library(plotly)

# Custom package
library(gmed)  # fbuckhold3/gmed on GitHub

# Package management
# Uses manifest.json for Posit Cloud Connect (not renv)
```

**Environment Variables**:
```r
# .Renviron file
REDCAP_API_URL=https://your-redcap-server.com/api/
REDCAP_API_TOKEN=your_secret_token_here
```

**Git Branch**: `rdm2-rebuild`

### File Organization Best Practices

**Module Files**:
- One module per file
- Clear naming: `mod_[module_name].R`
- UI function first, server function second
- Include inline documentation

**Utility Files**:
- Group related functions
- Clear naming: `[purpose]_helpers.R`
- Export functions used by multiple modules
- Include examples in comments

**Configuration Files**:
- Separate config from code
- Use list structures for complex configs
- Document all configuration options

**Example Structure**:
```r
# R/config/section_config.R
SECTION_CONFIG <- list(
  wellness = list(
    title = "Wellness & Progress",
    icon = "heart",
    required_fields = c("coach_wellness"),
    applicable_periods = 1:7,
    form_name = "coach_rev"
  ),
  evaluations = list(
    title = "Evaluations & Feedback",
    icon = "clipboard",
    required_fields = c("coach_evaluations", "coach_p_d_comments"),
    applicable_periods = 1:7,
    form_name = "coach_rev"
  )
  # ... other sections
)

get_section_config <- function(section_name) {
  SECTION_CONFIG[[section_name]]
}
```

---

## Development Patterns & Principles

### Core Development Principles

#### 1. Simplicity Over Complexity

**Principle**: Favor direct, straightforward approaches over elaborate multi-step workflows.

**Examples**:
- ✅ Direct table access with filtering
- ❌ Multi-step wizard interfaces
- ✅ Full-width layouts with essential info
- ❌ Complex sidebar navigation
- ✅ Progressive disclosure in single interface
- ❌ Separate pages for each section

**Rationale**: Reduces cognitive load, decreases development time, easier to maintain and debug.

#### 2. Modular Architecture

**Principle**: Separate concerns through clear module boundaries and responsibilities.

**Guidelines**:
- One module = one conceptual unit (section, navigation element, etc.)
- Modules communicate through reactive values, not global state
- Each module has clear inputs and outputs
- No module should directly depend on another module's internal state

**Benefits**: Easier testing, reusable components, cleaner code organization.

#### 3. Bulletproof Helper Functions

**Principle**: Helper functions should gracefully handle all edge cases and unexpected inputs.

**Requirements**:
- Automatic reactive value unwrapping
- Explicit NULL/NA handling
- Clear error messages
- Defensive programming
- Type checking for critical parameters

**Example Pattern**:
```r
my_helper_function <- function(param1, param2) {
  
  # 1. Unwrap reactive values
  p1 <- safe_reactive_value(param1)
  p2 <- safe_reactive_value(param2)
  
  # 2. Validate inputs
  if (is.null(p1) || is.null(p2)) {
    warning("my_helper_function: NULL parameters provided")
    return(NULL)
  }
  
  # 3. Type checking
  if (!is.numeric(p1)) {
    stop("my_helper_function: param1 must be numeric")
  }
  
  # 4. Business logic
  result <- calculate_something(p1, p2)
  
  # 5. Validate output
  if (is.null(result)) {
    warning("my_helper_function: calculation returned NULL")
    return(NA)
  }
  
  return(result)
}
```

#### 4. Iterative Debugging Approach

**Principle**: Debug systematically with diagnostic versions rather than attempting comprehensive fixes.

**Process**:
1. Create diagnostic version with extensive logging
2. Run and collect error messages
3. Analyze one error at a time
4. Fix root cause, not symptoms
5. Test fix thoroughly before moving to next issue
6. Keep git commits small and focused

**Benefits**: Prevents cascading issues, maintains working states, clearer commit history.

#### 5. Raw Data Format Preservation

**Principle**: Store data in raw format, translate only for display.

**Guidelines**:
- Always load with `raw_or_label = "raw"`
- Create translation layers for UI display
- Never store label format in reactive values
- Translate at render time, not at load time

**Rationale**: Raw format is canonical, ensures compatibility with other system components, enables proper checkbox handling.

### Code Quality Standards

#### Clear Naming Conventions

**Functions**:
- Action verbs: `calculate_`, `filter_`, `check_`, `get_`, `render_`
- Descriptive: `filter_resident_period_data` not `filter_data`
- Consistent: All similar functions use same prefix

**Variables**:
- Descriptive: `selected_resident` not `sr`
- Consistent: `current_period`, `previous_period` (not `cur_per`, `prev_per`)
- Type indicators: `_data`, `_list`, `_df` for clarity

**Modules**:
- Pattern: `mod_[module_name]_[ui/server]`
- Example: `mod_section_wellness_ui`, `mod_section_wellness_server`

#### Documentation Requirements

**Function Documentation**:
```r
#' Calculate REDCap Instance for Period
#'
#' Determines the correct repeating instance number for a given
#' resident level and period. Uses gmed package logic for consistency
#' with resident self-assessment system.
#'
#' @param level Integer, PGY level (1, 2, or 3)
#' @param period Integer, period number (1-7)
#' @param review_type String, "scheduled" or "adhoc"
#'
#' @return Integer, REDCap repeat instance number
#'
#' @examples
#' instance <- calculate_instance(level = 2, period = 3)
#' # Returns: 11
#'
calculate_instance <- function(level, period, review_type = "scheduled") {
  # Implementation
}
```

**Module Documentation**:
```r
# Module: Section Wellness
#
# Purpose: Displays resident wellness data from current and previous
#          periods and provides coach entry field for wellness notes.
#
# Inputs:
#   - app_data: Reactive value containing all REDCap data
#   - selected_resident: Reactive value with record_id
#   - current_period: Reactive value with period number
#   - previous_period: Reactive value with previous period (or NA)
#
# Outputs:
#   - Returns reactive list with:
#     - coach_wellness: Coach notes text
#     - completed: Boolean, section completion status
#
# Dependencies:
#   - utils/data_helpers.R (filter_resident_period_data)
#   - utils/reactive_helpers.R (safe_reactive_value)
```

#### Error Handling Standards

**User-Facing Errors**:
```r
# Show helpful, non-technical messages
if (is.null(selected_resident())) {
  showNotification(
    "Please select a resident to review",
    type = "warning"
  )
  return()
}
```

**Developer Errors**:
```r
# Provide detailed debugging information
if (nrow(filtered_data) == 0) {
  message <- sprintf(
    "filter_resident_period_data: No data found for record_id=%s, period=%d, form=%s",
    record_id, period, form_name
  )
  warning(message)
  return(NULL)
}
```

**API Errors**:
```r
# Capture and display REDCap API errors
tryCatch({
  result <- submit_to_redcap(data)
  if (!result$success) {
    showModal(modalDialog(
      title = "Submission Failed",
      p("Unable to submit review to REDCap."),
      p(strong("Error:"), result$error_message),
      p("Please contact support if this persists."),
      footer = modalButton("OK")
    ))
  }
}, error = function(e) {
  showModal(modalDialog(
    title = "System Error",
    p("An unexpected error occurred during submission."),
    p(strong("Technical details:"), e$message),
    footer = modalButton("OK")
  ))
})
```

### Testing Strategy

**Component Testing**:
- Test each module independently with mock data
- Verify UI renders correctly with various data states
- Check reactive dependencies fire correctly

**Integration Testing**:
- Test full flow: coach selection → resident selection → review → submission
- Verify period changes update all sections correctly
- Check completion status indicators update properly

**Edge Case Testing**:
- Empty data sets (new residents with no history)
- Archived residents
- Missing required fields
- Multiple coaches per resident
- Period boundary conditions (Period 7 → 1 transition)

---

## Testing & Validation

### Pre-Deployment Testing Checklist

**Data Loading**:
- [ ] Data loads with raw format successfully
- [ ] Archived residents excluded from active list
- [ ] Historical medians calculated correctly
- [ ] Level at time calculations accurate
- [ ] All forms present in loaded data

**Navigation**:
- [ ] Authentication/coach selection works
- [ ] Resident table displays correctly
- [ ] Completion indicators accurate
- [ ] Period override reloads data properly
- [ ] Navigation between sections smooth

**Review Interface**:
- [ ] All accordion sections render
- [ ] Previous period data displays correctly
- [ ] Current period data displays correctly
- [ ] Coach entry fields functional
- [ ] Progressive disclosure works
- [ ] gmed visualizations render properly

**Data Handling**:
- [ ] Checkbox fields parse correctly
- [ ] Translation layers work for display
- [ ] Reactive values unwrap properly
- [ ] NULL/NA cases handled gracefully
- [ ] Period-specific filtering accurate

**Submission**:
- [ ] Instance calculation correct
- [ ] All required fields included
- [ ] Form completion status set
- [ ] API submission succeeds
- [ ] Error handling displays properly
- [ ] Success modal shows correct info

**Post-Submission**:
- [ ] Data reloads after submission
- [ ] Completion status updates in table
- [ ] Form inputs clear properly
- [ ] Navigation returns to table
- [ ] Green check displays for completed review

### Common Issues & Solutions

**Issue**: "Error in filter_resident_period_data: argument is a function"

**Cause**: Reactive value passed as function instead of calling function

**Solution**: Use `safe_reactive_value()` helper in all data functions

---

**Issue**: "Checkbox field displays as numbers instead of labels"

**Cause**: Using label format instead of raw, or missing translation layer

**Solution**: Load with `raw_or_label = "raw"` and create translation layer

---

**Issue**: "No data displayed for previous period"

**Cause**: Period calculation incorrect or instance number wrong

**Solution**: Verify `calculate_previous_period()` logic and use `gmed::get_redcap_instance()`

---

**Issue**: "Milestones submitted to wrong form"

**Cause**: Milestone data must go to `milestone_entry`, not `coach_rev`

**Solution**: Separate milestone submission from main review submission

---

**Issue**: "Section modules not sourcing correctly"

**Cause**: Incorrect sourcing order or circular dependencies

**Solution**: Source in dependency order: utils → wrappers → sections → navigation

---

## Related Systems

### Resident Self-Assessment Application

**Repository**: `fbuckhold3/imslu-resident-self-assessment`

**Purpose**: Residents complete self-evaluations that coaches review in this dashboard

**Key Integration Points**:
- Uses identical period structure
- Submits to same REDCap forms (`s_eval`, `ilp`, `milestone_selfevaluation_c33c`)
- Period detection logic shared through gmed package
- Milestone subcompetency definitions identical

**Data Flow**:
```
Resident Self-Assessment App
  ↓ (submits to REDCap)
s_eval form, ilp form, milestone_selfevaluation_c33c form
  ↓ (loaded by)
Coaching Dashboard
```

### gmed Package

**Repository**: `fbuckhold3/gmed`

**Purpose**: Shared R package with visualization and data utilities

**Key Functions Used**:
- `load_rdm_complete()`: Load all REDCap data
- `get_current_period()`: Determine active period
- `get_redcap_instance()`: Calculate instance number
- `assessment_viz_ui/server()`: Assessment visualizations
- `mod_plus_delta_table_ui/server()`: Plus/delta feedback table
- `mod_scholarship_display_ui/server()`: Scholarship table
- `mod_questions_viz_ui/server()`: Conference questions
- Plus others for milestone displays, spider plots, etc.

**Update Considerations**:
- Package updates affect both resident app and coaching dashboard
- Test changes in both contexts
- Maintain backward compatibility

### REDCap Database

**Database**: Internal Medicine Residency RDM 2.0

**Data Dictionary**: `rdm2_data_dict_10_22_25.csv` (520 rows)

**Critical Forms**:
- `coach_rev`: Primary destination for coaching dashboard submissions
- `second_review`: Secondary reviewer destination
- `milestone_entry`: Coach milestone ratings (separate from main review)

**API Access**:
- Requires API token in environment variable
- Uses REDCapR package for submissions
- Must respect rate limits and best practices

---

## Appendix

### Period × Section Applicability Matrix

Quick reference for which sections appear in each period:

| Section | P1 | P2 | P3 | P4 | P5 | P6 | P7 |
|---------|----|----|----|----|----|----|-----|
| Wellness & Progress | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Evaluations & Feedback | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Learning & Board Prep | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Scholarship | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ |
| Career Planning | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ❌ |
| Milestones | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| Goals & ILP | ✅ | ✅ | ✅ | ✅ | ✅ | ❌ | ✅ |
| Summary | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

### Required Fields by Section

**Section 1: Wellness & Progress**
- `coach_wellness` (notes)

**Section 2: Evaluations & Feedback**
- `coach_evaluations` (notes)
- `coach_p_d_comments` (notes)

**Section 3: Learning & Board Preparation**
- `coach_step_board` (notes)

**Section 4: Scholarship**
- `coach_scholarship` (notes)

**Section 5: Career Planning** (Periods 1-5 only)
- `coach_career` (notes)

**Section 6: Milestones** (ALL PERIODS)
- All 23 `rep_*` fields (required)
- `rep_*_desc` fields (optional but recommended)

**Section 7: Goals & ILP** (Periods 1-5 only)
- `coach_goals` (notes)

**Section 8: Summary**
- No direct entry (validation only)

### Git Workflow

**Branch Structure**:
- `main`: Stable, production-ready code
- `rdm2-rebuild`: Active development branch for RDM 2.0 transition
- Feature branches: `feature/[feature-name]` off `rdm2-rebuild`

**Commit Guidelines**:
- Small, focused commits
- Clear commit messages with context
- Reference issue numbers if applicable
- Test before committing

**Merge Strategy**:
- Feature branches → `rdm2-rebuild` via pull request
- `rdm2-rebuild` → `main` when phase complete and tested

### Deployment

**Platform**: Posit Cloud Connect

**Requirements**:
- `manifest.json` file (not renv.lock)
- All dependencies in DESCRIPTION or manifest
- Environment variables configured in Posit Connect
- REDCap API token secured

**Deployment Process**:
1. Test thoroughly in development
2. Commit to `rdm2-rebuild` branch
3. Merge to `main` when ready
4. Push to GitHub
5. Deploy to Posit Connect from `main` branch
6. Verify environment variables
7. Test in production with limited users
8. Full rollout after validation

---

**Document Version**: 1.0  
**Last Updated**: December 2025  
**Maintainer**: Fred (Development Lead)  
**Repository**: fbuckhold3/imslu.coach.dash  
**Branch**: rdm2-rebuild  
**Shared Package**: fbuckhold3/gmed  
**Architecture Model**: fbuckhold3/imslu-resident-self-assessment
