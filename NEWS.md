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
