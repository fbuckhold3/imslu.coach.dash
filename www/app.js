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