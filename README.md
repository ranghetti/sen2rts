# sen2rts: Build and Analyse Sentinel-2 Time Series

`{sen2rts}` is an R library which helps to extract and manage time series
from Sentinel-2 archives created with the package [sen2r](https://sen2r.ranghetti.info/).

This package is currently under development (alpha version).

This functions are currently available and can be used in this order
to obtain a smoothed equally-weighted time series:
- `read_s2cube()` to build data cubes from Sentinel-2 archives;
- `extract_s2ts()` to build raw time series in `s2ts` format;
- `smooth_s2ts()` to smooth and filter time series;
- `fill_s2ts()` to fill non equally-temporally weighted time series.

An adequate documentation of these functions will be written in a future release.

See [issues](https://github.com/ranghetti/sen2rts/issues) to know which 
improvements are planned for future release
(issues can not be used for assistance and questions until the package is in alpha version).

## Credits

`{sen2rts}` is being developed by Luigi Ranghetti and Lorenzo Busetto
([IREA-CNR](http://www.irea.cnr.it)), and it is released under the [GNU
General Public License
version 3](https://www.gnu.org/licenses/gpl-3.0.html) (GPL‑3).
