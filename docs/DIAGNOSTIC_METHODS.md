# Diagnostic and screening methods

## Purpose

HydroStat diagnostic indicators are screening and visual-review tools. They are not official hydrological quality grades and do not automatically exclude a station, measurement, year or event.

## Architecture

The application separates diagnostics into:

- lightweight pre-calculated station summaries used by maps, filters and overview cards;
- detailed point-level calculations produced on demand for the selected station.

## Discharge-measurement screening

Current checks include:

- stage values equal to or below zero;
- discharge values equal to or below zero;
- repeated rounded stage values with variable discharge;
- repeated rounded discharge values with variable stage.

Repeated-value checks use minimum group-size and spread criteria to identify groups that deserve inspection.

## Rating-curve matching and residuals

Measurements are matched to rating-curve segments by:

- validity period;
- valid stage range.

The multiplicative diagnostic residual is:

```text
log_residual = log(Q_observed) - log(Q_rating)
```

Empirical residual envelopes are descriptive and are not official confidence intervals.

## Temporal-regime screening

The station-level baseline is a rating-like power law:

```text
Q = a * (H - h0)^b
```

where `Q` is discharge, `H` is stage, `h0` is an estimated diagnostic offset, and `a` and `b` are fitted coefficients.

The baseline is fitted only to valid positive stage and discharge measurements. Contiguous temporal changes are screened using log-residual behavior. Results indicate possible temporal structure requiring review; they are not definitive break classifications.

## Daily fluviometric consistency

When daily discharge, stage and rating curves are available in the session, screening includes:

- data coverage and gaps;
- discharge without stage and stage without discharge;
- discharge outside rating-curve periods;
- stage outside segment range;
- multiple applicable segments;
- generated-versus-source discharge differences;
- non-positive daily values.

A missing stage series produces a clearly partial assessment.

## Pluviometric consistency

Daily rainfall screening includes:

- missing or negative rainfall;
- unusually high values;
- source-status attention;
- long zero-rainfall sequences;
- suspicious repeated positive values;
- true duplicates at the same consistency level.

When raw and quality-controlled values exist for the same date, the quality-controlled value is preferred. The two levels are not treated as duplicates.

## Extreme-event screening

Annual maxima use October–September hydrological years. Annual low flows use civil years and complete moving-average windows of 1, 3, 7, 15 or 30 days. POT analysis is descriptive and uses thresholding and declustering without complete frequency modelling.

Flags are non-exclusive and are intended to focus documentary and visual review.
