# Batch File Archives

This directory contains archived batch conversion files (requests and results) with timestamps.

## File Naming Convention

- `batch-requests-YYYYMMDD-HHMMSS.jsonl` - Archived batch request files
- `batch-requests-customize-YYYYMMDD-HHMMSS.jsonl` - Archived customize batch requests
- `batch-results-YYYYMMDD-HHMMSS.jsonl` - Archived batch result files

## Purpose

- **Preserve history**: Keep old batch files for reference and debugging
- **Clean workspace**: Only one active batch file in `tools/` at a time
- **Easy cleanup**: Can delete old archives if disk space is needed

## Active Files

The active batch files are in `tools/`:
- `tools/batch-requests.jsonl` - Current batch requests for regular scripts (regenerated each time)
- `tools/batch-requests-customize.jsonl` - Current batch requests for customize scripts (regenerated each time)
- `tools/batch-results.jsonl` - Most recent batch results (default)
- `tools/batch-results-customize.jsonl` - Most recent customize batch results (optional)

**Note:** We can have multiple active batch request files and multiple active results files (one per batch type). Use `retrieve-batch.sh <batch-id> <results-file>` to specify a custom results filename.

## Automatic Archiving

Files are automatically archived when:
- Generating new batch requests (old requests of that type archived)
- Retrieving new batch results (old results archived)

Old files are moved here with timestamps before new ones are created.

## Manual Archiving

If you want to archive an old batch file manually:
```bash
# Archive old batch-requests.jsonl
mv tools/batch-requests.jsonl archive/batch/batch-requests-$(date +%Y%m%d-%H%M%S).jsonl

# Archive old batch-requests-customize.jsonl  
mv tools/batch-requests-customize.jsonl archive/batch/batch-requests-customize-$(date +%Y%m%d-%H%M%S).jsonl

# Archive old batch-results.jsonl
mv tools/batch-results.jsonl archive/batch/batch-results-$(date +%Y%m%d-%H%M%S).jsonl

# Archive old batch-results-customize.jsonl
mv tools/batch-results-customize.jsonl archive/batch/batch-results-customize-$(date +%Y%m%d-%H%M%S).jsonl
```

