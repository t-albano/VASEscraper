Data collected from [taea website](https://www.taea.org/vase/gallery.cfm).

Description of variables in `VASE_cleandata_newest.csv` used in Dashboard:

* **Student:** String. Student name (first name last name) as listed in website. Students who scored a 4
at state level were listed. Otherwise, the website did not list student.

* **Title:** String. Title of piece as listed in website.

* **School:** String. Name of TEA school. Not all schools in website where utilized. Schools utilize
had to be a public TEA school that could be fuzzy joined with `TEA22_rawdata.csv` and could be identified
using district name and region number. If school spanned multiple districts, or schools with same name but no
further distinctions were ommitted.

* **District:** String. Name of TEA district associated with school. Collected from `TEA22_rawdata.csv`.

* **Enrolled:** Integer. Number of students enrolled in school, according to `TEA22_rawdata.csv`.

* **Year:** Integer. Year VASE competition was held.

* **Art_Region:** String. TEA region number with possibly a N, E, S, W indicator to separate regions.

* **Dimension:** String. Indicates if piece competed as a `2D` or `3D` piece.

* **Division:** Integer. Division students competed in. Division is based on number of credits that 
student has in high school art. However, if student is taking or has took an AP art, the student is immediately
pushed to Division 4. 

* **Gold_Seal:** Integer. `0` indicated student did not win gold seal, `1` indicates student won gold seal. Top
10% of artworks that scored a 4 (according to the selected judges) receive this award within each division and dimension.

* **URL:** String. Link to student page with artwork, as found on TAEA website.

* **OBJECTID:** Integer. Linked district ID number found in GEO data from TEA website, for geospatial plotting.
