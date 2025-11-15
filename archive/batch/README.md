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
- `tools/batch-requests.jsonl` - Current batch requests (regenerated each time)
- `tools/batch-requests-customize.jsonl` - Current customize batch requests
- `tools/batch-results.jsonl` - Most recent batch results

## Automatic Archiving

Files are automatically archived when:
- Generating new batch requests (old requests archived)
- Retrieving new batch results (old results archived)

Old files are moved here with timestamps before new ones are created.

