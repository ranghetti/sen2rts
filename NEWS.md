# Version 0.3.1

## Implementations
- `fit_curve()`: add value `"no"` for argument `fit` (useful to skip interpolation to extract phenological metrics with TRS).
- New function `aggregate_pheno()` to aggregate phenological metrics.
- New function `assign_season()` to assign a season label to cycles.
- Change SG window: from n. images to days. In this way there are no differences among periods with/without S2B (when only S2A exists the curve is less smoothed, but the window is the same in terms of days).
- Allow multiple `fit` values in `fit_curve()`, so to use the secondary method in case of failure of the first.
- Rewrite `cut_cycles()` to follow Meroni et al. (2021).

## Minor changes
- Semantic change: season -> cycle, involving name of functions (`cut_seasons()` -> `cut_cycles()`) and parameters.
- Patch `PhenoTrs()` to ensure `sos` < `pop` < `eos`.
- `year` is added to outputs of functions since `cut_cycles()`.
- Allow specifying a maximum number of seasons per year.
- Adapt plot methods.
- Fixes.


# Version 0.3.0

- New functions to reach [milestone 2](https://github.com/ranghetti/sen2rts/milestone/2):
    - `cut_seasons()`: cut Sentinel-2 time series into separate seasons, detecting dates of cuts and of peaks;
    - `fit_curve()`: fit a curve using a parametric function  from `phenopix::greenProcess()`;
    - `extract_pheno()`: extract phenological metrics from a fitted time series using methods from `phenopix::PhenoExtract()`;
    - `filter_seasons()`: filter detected seasons basing on dates of begin / end / peak, and associate each season with a reference year.
- Update plot methods to integrate phenological metrics and fitted curves (this will be improved).


# Version 0.2.2

- Savitzky-Golay filtered can be repeated to better follow higher values.
- CLD values are no more used "as are", but rescaled basing on SCL weights 
    assigned to cloudy classes.
- Weight values > `qa_min` are no more rescaled to 0-1; values with < `qa_min` 
    are no more removed, but replaced with a linear interpolation.
- `scl_weights()` default values are slightly changed according to previous change.
- Spikes are removed before filtering on `qa`.
- In `extract_s2ts()` on polygon features, best values of each polygon can be 
    used instead of the weighted average.


# Version 0.2.1

- Add arguments `max_na_days` and `method` to `fill_s2ts()`
- Improvements in class `s2ts`


# Version 0.2.0

First package release (still in alpha version) including functions planned
for [first milestone](https://github.com/ranghetti/sen2rts/milestone/1):

- `read_s2cube()` to build data cubes from Sentinel-2 archives;
- `extract_s2ts()` to build raw time series in `s2ts` format;
- `smooth_s2ts()` to smooth and filter time series;
- `fill_s2ts()` to fill non equally-temporally weighted time series.
