# IMSLU Coaching Dashboard - Production Deployment Guide

## Overview

This guide covers deploying the coaching dashboard to production on Posit Connect.

## Pre-Deployment Checklist

### 1. Authentication

**Development Mode (Login Bypass):**
- Set environment variable: `BYPASS_LOGIN=true` in `.Renviron`
- App will skip authentication and proceed directly to coach selection

**Production Mode (Secure Authentication):**
- DO NOT set `BYPASS_LOGIN` environment variable (or set to `false`)
- App will require proper authentication via the login module
- Users must authenticate before accessing the dashboard

### 2. Environment Variables

Required environment variables in Posit Connect:

```
REDCAP_API_URL=https://your-redcap-server.com/api/
REDCAP_API_TOKEN=your_secret_token_here
```

**DO NOT SET IN PRODUCTION:**
```
BYPASS_LOGIN=true  # Only for development!
```

### 3. Dependencies

All dependencies are managed via `manifest.json` (not renv).

Key packages:
- `shiny`
- `bslib`
- `shinydashboard`
- `dplyr`, `tidyr`, `stringr`
- `DT`
- `plotly`
- `REDCapR`
- `gmed` (GitHub: fbuckhold3/gmed)

### 4. Branch Strategy

- **Development branch:** `claude/fix-review-interface-syntax-01XLQwMqrvEkTNTrFa3GULNQ`
- **Production branch:** `main`

**Before deploying to production:**
1. Merge development branch to `main`
2. Tag the release: `git tag v1.0.0`
3. Push tags: `git push --tags`

## Deployment Steps

### Step 1: Prepare Code

```bash
# Ensure you're on the correct branch
git checkout main

# Pull latest changes
git pull origin main

# Verify no debug statements remain
grep -r "DEBUG\|message(sprintf" R/
```

### Step 2: Test Locally

```bash
# Set environment variables (create .Renviron if needed)
echo "REDCAP_API_URL=https://redcap.wustl.edu/api/" > .Renviron
echo "REDCAP_API_TOKEN=YOUR_TOKEN_HERE" >> .Renviron

# Test with authentication enabled (production mode)
# DO NOT add BYPASS_LOGIN to .Renviron

# Run the app
R -e "shiny::runApp()"
```

### Step 3: Deploy to Posit Connect

```bash
# Using rsconnect package
R -e "rsconnect::deployApp(appDir = '.', appName = 'imslu-coach-dash')"
```

### Step 4: Configure in Posit Connect

1. Navigate to Content Settings
2. Set Environment Variables:
   - `REDCAP_API_URL`: Your REDCap API endpoint
   - `REDCAP_API_TOKEN`: Secure API token
3. Verify `BYPASS_LOGIN` is NOT set
4. Set Access Control (who can view the app)
5. Configure Runtime Settings (if needed)

### Step 5: Post-Deployment Testing

Test the following workflows:

#### Authentication
- [ ] Login page displays
- [ ] Authentication required (no bypass)
- [ ] Invalid credentials rejected

#### Coach Selection
- [ ] Coach dropdown populates
- [ ] Coach statistics display correctly
- [ ] Navigation to resident table works

#### Resident Table
- [ ] Residents filtered by coach assignment
- [ ] Completion indicators accurate
- [ ] Period detection correct (PGY2 = Mid PGY2, etc.)
- [ ] Both primary and secondary roles displayed

#### Primary Review Interface
- [ ] All sections display
- [ ] Milestone entry shows CURRENT period self-assessment
- [ ] Spider plots render correctly
- [ ] Submission to REDCap successful
- [ ] Completion status updates after submission

#### Second Review Interface (for secondary reviewers)
- [ ] Dual spider plots display (self and coach)
- [ ] Coach ILP comments display
- [ ] Approval form functional
- [ ] Conditional milestone comments work
- [ ] Submission to second_review form successful

## Troubleshooting

### Issue: Everyone showing "Mid PGY3" period

**Cause:** `grad_yr` field not numeric
**Fix:** Already implemented - `as.numeric(grad_year)` conversion

### Issue: Login bypass active in production

**Cause:** `BYPASS_LOGIN` environment variable set
**Fix:** Remove `BYPASS_LOGIN` from Posit Connect environment variables

### Issue: REDCap submission fails

**Causes:**
- Invalid API token
- Field name mismatch
- Network connectivity

**Check:**
- Environment variables set correctly
- `gmed::calculate_pgy_and_period()` returns valid results
- Instance numbers calculated correctly

## Rollback Procedure

If issues occur in production:

```bash
# Revert to previous stable version
git checkout v0.9.0  # Or previous stable tag

# Redeploy
R -e "rsconnect::deployApp()"
```

## Monitoring

**Check regularly:**
- Error logs in Posit Connect
- Submission success rates to REDCap
- User feedback on period detection accuracy
- Authentication failures

## Support

**Developer:** Fred
**Repository:** https://github.com/fbuckhold3/imslu.coach.dash
**Branch:** main
**Related Package:** fbuckhold3/gmed

## Version History

### v1.0.0 (Current)
- Fixed period calculation (use expected period, not most recent with data)
- Fixed milestone entry to show current period self-assessment
- Added second review interface for secondary reviewers
- Cleaned up debug logging
- Production-ready with proper authentication

### Features Implemented:
✅ Coach assignment filtering
✅ Resident selection table with completion tracking
✅ Period detection based on PGY level and current date
✅ Primary review interface with milestone entry
✅ Second review interface with dual spider plots
✅ REDCap integration for form submission
✅ Modern UI design matching self-assessment app

### Known Limitations:
- Ad hoc reviews not yet implemented
- Some accordion sections (2-8) still in development
- Historical trend visualizations planned for future
