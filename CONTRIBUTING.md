# Contributing

Contributions that improve correctness, portability, documentation, accessibility or reproducibility are welcome.

## Before opening an issue

1. Confirm that the problem occurs in the current public version.
2. Record the operating system, R version and relevant package versions.
3. Provide the selected station code only when it is public ANA information.
4. Describe the input format without attaching confidential files.
5. Remove credentials, tokens, local paths and personal data.

## Bug reports

A useful report includes:

- steps to reproduce;
- expected result;
- observed result;
- relevant warning or error text;
- whether the problem occurs with bundled data, uploaded data or an ANA download;
- a minimal non-sensitive example when possible.

## Pull requests

- Keep changes localized to the responsible runtime or pipeline file.
- Preserve user-facing labels in Portuguese.
- Write code comments and object/function names in English.
- Do not install packages inside runtime or pipeline scripts.
- Do not persist session credentials, tokens or complete downloaded time series.
- Do not change database contracts or analytical behavior without documented justification and regression testing.
- Update documentation when behavior, dependencies or data contracts change.

## Scientific interpretation

Diagnostic indicators are screening aids, not official quality grades. Changes to thresholds, formulas or hydrological conventions require explicit documentation and validation.
