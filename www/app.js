// IMSLU Coaching App JavaScript

$(document).ready(function() {
  // Update progress bar when tab changes
  $("#primary_review_tabs").on("change", function() {
    updateProgressBar();
  });
  
  // Initialize tooltips
  $('[data-toggle="tooltip"]').tooltip();
  
  // Make tables responsive
  $('.dataTables_wrapper').addClass('table-responsive');
});

// Function to update progress bar based on current tab
function updateProgressBar() {
  const tabs = [
    "pre_review", "wellness", "evaluations", "knowledge", 
    "scholarship", "ilp", "career", "summary", "milestones"
  ];
  
  const currentTab = $('#primary_review_tabs .nav-link.active').attr('data-value');
  const currentIndex = tabs.indexOf(currentTab);
  
  if (currentIndex !== -1) {
    const progressPercentage = Math.round((currentIndex / (tabs.length - 1)) * 100);
    
    // Update the progress bar
    $("#review_progress_bar")
      .css("width", progressPercentage + "%")
      .attr("aria-valuenow", progressPercentage);
      
    // Update step text
    $("#current_step_text").text(
      "Step " + (currentIndex + 1) + " of " + tabs.length + ": " + 
      $('#primary_review_tabs .nav-link.active').text()
    );
    
    // Show/hide navigation buttons
    if (currentIndex === tabs.length - 1) {
      $("#next_tab").hide();
      $("#submit_primary_review").show();
    } else {
      $("#next_tab").show();
      $("#submit_primary_review").hide();
    }
  }
}

// Enhances validation of forms before submission
function validateSubmission() {
  // This is a placeholder for form validation logic
  // You can implement field validation here
  const requiredFields = [
    "summary_comments",
    "milestone_goals_comments",
    "career_comments"
  ];
  
  let isValid = true;
  let missingFields = [];
  
  requiredFields.forEach(field => {
    const value = $("#" + field).val();
    if (!value || value.trim() === "") {
      isValid = false;
      missingFields.push(field);
      $("#" + field).addClass("is-invalid");
    } else {
      $("#" + field).removeClass("is-invalid");
    }
  });
  
  return {
    isValid: isValid,
    missingFields: missingFields
  };
}

// Handle coach selection change
function handleCoachSelection() {
  const selectedCoach = $("#coach_name").val();
  
  if (selectedCoach) {
    // Show the residents table for this coach
    $("#residents_table_container").show();
  } else {
    // Hide the residents table if no coach selected
    $("#residents_table_container").hide();
  }
}

js_code_to_add <- '
// Handle secondary review form validation
function validateSecondaryReview() {
  const comments = $("#secondary_coach_comments").val();
  const approval = $("input[name=\'approve_milestones\']:checked").val();
  const concerns = $("#milestone_concerns").val();
  
  let isValid = true;
  let errors = [];
  
  if (!comments || comments.trim() === "") {
    errors.push("Comments are required");
    $("#secondary_coach_comments").addClass("is-invalid");
  } else {
    $("#secondary_coach_comments").removeClass("is-invalid");
  }
  
  if (!approval) {
    errors.push("Milestone approval is required");
  }
  
  if (approval === "no" && (!concerns || concerns.trim() === "")) {
    errors.push("Explanation of concerns is required when disagreeing with milestones");
    $("#milestone_concerns").addClass("is-invalid");
  } else {
    $("#milestone_concerns").removeClass("is-invalid");
  }
  
  return {
    isValid: errors.length === 0,
    errors: errors
  };
}

// IMSLU Coaching App JavaScript - Updated with Intern Intro Support

$(document).ready(function() {
  // Update progress bar when tab changes
  $("#primary_review_tabs").on("change", function() {
    updateProgressBar();
  });
  
  // Initialize tooltips
  $('[data-toggle="tooltip"]').tooltip();
  
  // Make tables responsive
  $('.dataTables_wrapper').addClass('table-responsive');
});

// Function to update progress bar based on current tab
function updateProgressBar() {
  const tabs = [
    "pre_review", "wellness", "evaluations", "knowledge", 
    "scholarship", "ilp", "career", "summary", "milestones"
  ];
  
  const currentTab = $('#primary_review_tabs .nav-link.active').attr('data-value');
  const currentIndex = tabs.indexOf(currentTab);
  
  if (currentIndex !== -1) {
    const progressPercentage = Math.round((currentIndex / (tabs.length - 1)) * 100);
    
    // Update the progress bar
    $("#review_progress_bar")
      .css("width", progressPercentage + "%")
      .attr("aria-valuenow", progressPercentage);
      
    // Update step text
    $("#current_step_text").text(
      "Step " + (currentIndex + 1) + " of " + tabs.length + ": " + 
      $('#primary_review_tabs .nav-link.active').text()
    );
    
    // Show/hide navigation buttons
    if (currentIndex === tabs.length - 1) {
      $("#next_tab").hide();
      $("#submit_primary_review").show();
    } else {
      $("#next_tab").show();
      $("#submit_primary_review").hide();
    }
  }
}

// Enhances validation of forms before submission
function validateSubmission() {
  // This is a placeholder for form validation logic
  // You can implement field validation here
  const requiredFields = [
    "summary_comments",
    "milestone_goals_comments",
    "career_comments"
  ];
  
  let isValid = true;
  let missingFields = [];
  
  requiredFields.forEach(field => {
    const value = $("#" + field).val();
    if (!value || value.trim() === "") {
      isValid = false;
      missingFields.push(field);
      $("#" + field).addClass("is-invalid");
    } else {
      $("#" + field).removeClass("is-invalid");
    }
  });
  
  return {
    isValid: isValid,
    missingFields: missingFields
  };
}

// Handle coach selection change
function handleCoachSelection() {
  const selectedCoach = $("#coach_name").val();
  
  if (selectedCoach) {
    // Show the residents table and other dashboard elements
    $("#residents-table-section").show();
    $("#coaching-actions-section").show();
  } else {
    // Hide elements when no coach is selected
    $("#residents-table-section").hide();
    $("#coaching-actions-section").hide();
  }
}

// ===============================================
// INTERN INTRO COACHING SESSION FUNCTIONS
// ===============================================

// Custom message handler for updating element content
Shiny.addCustomMessageHandler("updateElement", function(message) {
  const element = document.getElementById(message.id);
  if (element) {
    element.innerHTML = message.content;
  } else {
    console.warn("Element not found:", message.id);
  }
});

// Function to validate intern intro coaching form
function validateInternIntroForm() {
  const requiredFields = [
    "coach_intro_back_input",
    "coach_coping_input", 
    "coach_wellness_input",
    "coach_ls_and_topic_input",
    "coach_summary_input"
  ];
  
  let isValid = true;
  let missingFields = [];
  
  requiredFields.forEach(fieldId => {
    const field = document.getElementById(fieldId);
    if (field) {
      const value = field.value.trim();
      if (!value) {
        isValid = false;
        missingFields.push(fieldId);
        field.classList.add("is-invalid");
      } else {
        field.classList.remove("is-invalid");
      }
    }
  });
  
  // Check completion checkbox
  const completionBox = document.getElementById("intern_intro_complete");
  if (completionBox && !completionBox.checked) {
    isValid = false;
  }
  
  return {
    isValid: isValid,
    missingFields: missingFields
  };
}

// Add real-time validation for intern intro form
$(document).on('input change', '#coach_intro_back_input, #coach_coping_input, #coach_wellness_input, #coach_ls_and_topic_input, #coach_summary_input, #intern_intro_complete', function() {
  const validation = validateInternIntroForm();
  const submitButton = document.getElementById("submit_intern_intro");
  
  if (submitButton) {
    if (validation.isValid) {
      submitButton.disabled = false;
      submitButton.classList.remove("btn-secondary");
      submitButton.classList.add("btn-success");
    } else {
      submitButton.disabled = true;
      submitButton.classList.remove("btn-success");
      submitButton.classList.add("btn-secondary");
    }
  }
});

// Enhanced table row selection for resident dashboard
$(document).on('click', '.residents-table tbody tr', function() {
  // Remove previous selection
  $('.residents-table tbody tr').removeClass('table-primary');
  
  // Add selection to clicked row
  $(this).addClass('table-primary');
  
  // Enable action buttons
  $('#start_coaching_session').prop('disabled', false);
  $('#start_intern_intro_session').prop('disabled', false);
  $('#start_secondary_review').prop('disabled', false);
});

// Show/hide appropriate action buttons based on period
$(document).on('change', '#coach_residents_table_rows_selected', function() {
  const selectedRow = $(this).val();
  if (selectedRow && selectedRow.length > 0) {
    // You can add logic here to show/hide buttons based on the selected resident's period
    // For example, only show intern intro button for intern intro periods
  }
});

// Smooth scrolling for navigation within the intern intro page
$(document).on('click', 'a[href^="#"]', function(e) {
  e.preventDefault();
  
  const target = $(this.getAttribute('href'));
  if (target.length) {
    $('html, body').animate({
      scrollTop: target.offset().top - 100
    }, 500);
  }
});

// Auto-resize textareas based on content
$(document).on('input', 'textarea', function() {
  this.style.height = 'auto';
  this.style.height = (this.scrollHeight + 5) + 'px';
});

// Add confirmation before leaving page if form has unsaved changes
window.addEventListener('beforeunload', function(e) {
  // Check if we're on the intern intro page and have unsaved changes
  if (document.getElementById('intern-intro-session-page') && 
      document.getElementById('intern-intro-session-page').style.display !== 'none') {
    
    const formFields = [
      'coach_intro_back_input',
      'coach_coping_input', 
      'coach_wellness_input',
      'coach_ls_and_topic_input',
      'coach_summary_input'
    ];
    
    let hasContent = false;
    formFields.forEach(fieldId => {
      const field = document.getElementById(fieldId);
      if (field && field.value.trim()) {
        hasContent = true;
      }
    });
    
    if (hasContent) {
      e.preventDefault();
      e.returnValue = '';
      return '';
    }
  }
});

// Initialize any additional components when page loads
$(document).ready(function() {
  // Initialize any date pickers, select2 dropdowns, etc.
  
  // Add keyboard shortcuts
  $(document).keydown(function(e) {
    // Ctrl+S to save (could trigger submit button)
    if (e.ctrlKey && e.keyCode === 83) {
      e.preventDefault();
      
      // Check which page we're on and trigger appropriate save action
      if (document.getElementById('intern-intro-session-page') && 
          document.getElementById('intern-intro-session-page').style.display !== 'none') {
        const submitButton = document.getElementById('submit_intern_intro');
        if (submitButton && !submitButton.disabled) {
          submitButton.click();
        }
      }
    }
  });
});