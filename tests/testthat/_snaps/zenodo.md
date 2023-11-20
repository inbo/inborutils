# download_zenodo() works for a single-file record

    Code
      download_zenodo(doi = "10.5281/zenodo.3784149", path = zenodo_dir)
    Message
      Will download 1 file (total size: 32.5 KiB) from https://doi.org/10.5281/zenodo.3784149 (Distribution of the Natura 2000 habitat type 7220 (Cratoneurion) in Flanders and Brussels Capital Region, Belgium (version 2020); version: habitatsprings_2020v2)
      
      
      Verifying file integrity...
      
      habitatsprings.geojson was downloaded and its integrity verified (md5sum: 64c3db07d17274da047b3962aab28e80)

# download_zenodo() works for a GitHub code record

    Code
      download_zenodo(doi = "10.5281/zenodo.7335805", path = zenodo_dir)
    Message
      Will download 1 file (total size: 236.7 KiB) from https://doi.org/10.5281/zenodo.7335805 (R package n2khab: providing preprocessed reference data for Flemish Natura 2000 habitat analyses; version: 0.8.0)
      
      
      Verifying file integrity...
      
      n2khab-v0.8.0.zip was downloaded and its integrity verified (md5sum: 25fb33360d257c085bce567da8f6a2cb)

# download_zenodo() works for a multi-file record

    Code
      download_zenodo(doi = "10.5281/zenodo.4420858", path = zenodo_dir)
    Message
      Will download 4 files (total size: 534.5 KiB) from https://doi.org/10.5281/zenodo.4420858 (Redistribution of the Natura 2000 habitat map of Flanders, partim habitat type 3260 (version 1.7); version: habitatstreams_v1.7)
      
      
      Verifying file integrity...
      
      habitatstreams.dbf was downloaded and its integrity verified (md5sum: f66ddddacc9511133cc02d8c1960a917)
      habitatstreams.shx was downloaded and its integrity verified (md5sum: e7725c8267ed671f3e5f09c5fcc68bff)
      habitatstreams.shp was downloaded and its integrity verified (md5sum: 5c94b58c9dc7809c4eeeaf660aa3323c)
      habitatstreams.prj was downloaded and its integrity verified (md5sum: f881f61a6c07741b58cb618d8bbb0b99)

# download_zenodo() can work sequentially for a multi-file record

    Code
      download_zenodo(doi = "10.5281/zenodo.4420858", path = zenodo_dir, parallel = FALSE)
    Message
      Will download 4 files (total size: 534.5 KiB) from https://doi.org/10.5281/zenodo.4420858 (Redistribution of the Natura 2000 habitat map of Flanders, partim habitat type 3260 (version 1.7); version: habitatstreams_v1.7)
      
      
      Verifying file integrity...
      
      habitatstreams.dbf was downloaded and its integrity verified (md5sum: f66ddddacc9511133cc02d8c1960a917)
      habitatstreams.shx was downloaded and its integrity verified (md5sum: e7725c8267ed671f3e5f09c5fcc68bff)
      habitatstreams.shp was downloaded and its integrity verified (md5sum: 5c94b58c9dc7809c4eeeaf660aa3323c)
      habitatstreams.prj was downloaded and its integrity verified (md5sum: f881f61a6c07741b58cb618d8bbb0b99)

