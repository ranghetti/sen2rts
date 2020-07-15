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
