## R CMD check results

0 errors | 0 warnings | 0 notes

Tested on: Windows 11 x64, R 4.5.3 (local)

## Notes on CRAN incoming check

The remote check shows a NOTE about the GitHub URL returning 504 Gateway
Timeout. This is a transient network issue during the check, not a problem
with the URL itself. The repository https://github.com/lizbettinag-dotcom/paraguaydata
is publicly accessible.

## Tests

- 218 tests pass locally (0 failures, 0 warnings)
- Tests requiring internet access use skip_on_cran() / skip_if_offline()
- All internet-dependent tests are properly skipped on CRAN

## External data sources

This package accesses the following external URLs:

- https://www.ine.gov.py — National Statistics Institute of Paraguay
  (EPHC survey data, geographic codes, IPM data). Publicly available,
  free of charge, licensed for public use.
- https://mec.gov.py/nada — Ministry of Education catalog (metadata only).

All functions accessing external URLs use skip_on_cran() in tests.
PISA 2022 data is included as internal datasets — no internet required.
MEC data functions work with user-provided local files — no internet required.

## Spell check

Package documentation is written in Spanish (the target audience is
Spanish-speaking researchers in Paraguay and Latin America). All words
flagged by spel
