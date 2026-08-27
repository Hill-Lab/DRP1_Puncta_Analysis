# DRP1 Puncta Analysis scripts and macros

Nikon JOBS and GA3 files, ImageJ (FIJI) macros, and R scripts used in the submitted manuscript "Pathogenic DRP1 variants reveal a role for biomolecular condensation in mitochondrial fission"

*All resources in this repository were contributed by **KA Ross***


## Nikon JOBS and GA3 files
> These files can only be opened and edited on valid Nikon-licensed software, specifically requiring the **NIS-Elements High-Content (HC)** or **Advanced Research (AR)** packages. Additionally, these scripts access and **override internal microscope variables** used to position the stage in a manner that **bypasses certain safety limits**. As such, these scripts should be utilized with **extreme caution** and executed under **active surveillance** to prevent damage to sensitive microscopy equipment. The Hill Lab is not responsible for any damage incurred through operation of these scripts, as outlined in the **MIT License** disclosed in this repository.

*for autonomous imaging and single-cell mask segmentation*

1. *405-488-561-threshold_PFS-capture-loop_24well-randomized.bin*  
The JOBS workflow that defines autonomous microscope operation. This script was routinely modified for each run (saving a local copy), ensuring that variables were mapped appropriately depending on the dimensions and well count of the plate to be imaged. Microscope laser parameters and camera settings are included in this workflow, although these might not translate to all microscope systems and will need to be altered accordingly. Nikon software/hardware updates may have altered key functionality of this JOBS workflow since its last operation (Nov. 2023), and expert Nikon support may be required to implement properly. 

2. *405-488-561_detect.ga3*  
A helper GA3 script (used in the main JOBS workflow above) that ensures appropriate stains and immunofluorescent signals are detected in the field of view before defining an image capture region.

3. *DAPI_detect.ga3*  
A helper GA3 script (used in the main JOBS workflow above) that counts nuclei in the field of view before defining an image capture region.

4. *Segment_MAXIP_DIC_DAPI.ga3*  
The primary GA3 workflow that handles single-cell segmentation (of images captured via the main JOBS workflow above) through nuclei object watershedding within cell boundaries defined by repeated contrast enhancement/distortion of differential interference contrast (DIC) images. Alternative segmentation strategies exist outside of the Nikon ecosystem and are more accessible, but will likely require modifying the provided ImageJ (FIJI) macros.


## ImageJ (FIJI) macros
> The first macro provided in this section will **only** work with valid GA3 mask files produced by the Nikon scripts provided above. An alternate version of this ImageJ macro functions with hand-drawn single-cell segmentations instead of GA3 masks and is the second macro provided in this section. DRP1 and TOM20 thresholds defined in the macro are likely not accurate for images captured with different settings or microscopes and should be updated accordingly. The <a href="https://imagej.net/plugins/3d-imagej-suite/">3D ImageJ Suite</a> plugin is required for segmenting and parsing subcellular punctate structures, if selected at runtime. Note that the 3D segmentation functions of this plugin will **fail** (but should not crash the macro) if performed on images lacking z-stacks. Similarly, Java Exception logs are produced when attempting to perform 3D measurements on failed 3D segmentations (or images without sufficient signal).

> These macros require precise file and folder naming conventions, indicated in comments within the code. These macros are quite memory-hungry when running on large datasets. Monitor system memory usage when running these macros and/or process data in smaller batches on computers with less system memory. Alternatively, these macros may benefit from being run on a high-performance computing cluster, access permitting. These macros are prone to crashing during operation, likely related to high memory utilization or instabilities caused by the 3D ImageJ Suite plugin (not validated). Fortunately, all data outputs are saved as they are processed (except for the combined MaxIP stack of all cropped cells, which is saved only after all images have been processed). If the macro crashes, restart it with the same parameters, excluding the raw data mapped to completed outputs, to continue processing. Updated versions of these macros will be provided if a solution to the instabilities is discovered.

*to process, crop, and compute various metrics from single-cell masks*

1. *imageProcess_SingleCellCrop_fromGA3Masks_coloc2_quantMetrics_punctaParse_v2.10.ijm*  
The master ImageJ macro responsible for generating single cells from GA3 masks while gathering and computing various metrics as specified by user input. The parameters for all quantification methods are **hard-coded** in their function calls, but could be modified to allow user input.

2. *imageProcess_SingleCellCrop_fromHandDrawnROIs_coloc2_quantMetrics_punctaParse_v2.00.ijm*  
Largely identical to the above macro, but operates using pre-generated single-cell ROIs (either hand-drawn or an analogous segmentation method).

3. *imageAnalysis_MFI_Objects_Background.ijm*  
This macro does not warrant the memory utilization and crash warnings outlined above. This macro processes fluorescent images of *in vitro* phase separation data and calculates various field metrics at multiple thresholds. Resulting data outputs were manually reformatted for use in continued processing.


## R scripts 

> These scripts were rarely executed in full, and were instead run in sections. This was done primarily to ensure that all data formatting, organization, and merging proceeded without error, and secondarily to reduce RStudio crashes that occurred with consecutive instructions on massive data frames (some containing upwards of 10 million rows). Most of these crashes appear to be limitations with RStudio itself, although several instances did result in a total system restart (BSoD). All plots were run as individual code sections and manually saved (to adjust and update plotting parameters at runtime where necessary). The input directories and files utilized in these scripts are generated automatically (if selected at runtime) by the ImageJ macros above.

> Some of these R scripts were used in the parsing and analysis of MitoGraph data. For more information regarding MitoGraph, please view the main <a href="https://github.com/vianamp/MitoGraph">MitoGraph repository</a>, our other <a href="https://github.com/Hill-Lab/MitoGraph-Contrib-RScripts">MitoGraph scripts</a>, and our published data in "<a href="https://www.sciencedirect.com/science/article/pii/S0003269718301921?via%3Dihub">*Methods for imaging mammalian mitochondrial morphology: A prospective on MitoGraph*</a>".

*to parse, format, merge, and organize single-cell data to generate violin plots and perform statistical analyses*

1. *quant_stats_parser.R*  
Parses field quantification data in *.csv* files (obtained for each cell via the ImageJ "Measure" function called in the above ImageJ macros) and combines them into a single *.xlsx* file. Separate versions of this file will exist for data parsed from all z-slices and MaxIP images.

2. *quant_formatter.R*  
Formats the combined data from *quant_stats_parser.R* with additional identifying information, computes additional metrics, and saves as a *.xlsx* file.

3. *dataset_merge_cells.R*  
Merges experimental replicates of the cellular quantification data generated by the above two scripts. Additionally used to produce violin plots, compute summary metrics, and perform statistical analyses.

4. *coloc2_parser_v3.R*  
Parses and formats colocalization data in *.csv* files (obtained for each cell via the "coloc2" function called in the above ImageJ macros) and combines them into a single *.xlsx* file. Separate versions of this file will exist for data parsed from all z-slices and MaxIP images.

5. *dataset_merge_coloc.R*  
Merges experimental replicates of the colocalization data generated by the above script. Additionally used to produce violin plots, compute summary metrics, and perform statistical analyses.

6. *punctaParse_masks_3Dthresholding_parser.R*  
Parses and formats all 3D-segmented puncta data (obtained for each cell and masked region via the "3D ImageJ Suite" plugin utilized in the above ImageJ macros) and combines them into a single *.xlsx* file. Separate versions of this file will exist for 3D Intensity, 3D Compactness, and 3D Volume data. These files can be exceptionally large (depending on dataset size) and may take several **minutes** to open and read into RStudio memory. The likelihood of crashes occurring when attempting to open and read these files increases as they contain more data. Saving copies of these files is recommended to prevent data loss/corruption that could occur with frequent crashes.

7. *dataset_merge_punctaParse_masks_3Dthresholding.R*  
Merges experimental replicates of the 3D-segmented puncta data generated by the above script. Additionally used to produce violin plots, compute summary metrics, and perform statistical analyses. The same crashing warnings made above also apply to this script.

8. *mitograph_parser_v1.R*  
Requires a "MitoGraph_output" directory (generated by MitoGraph software).
Parses, filters, and formats all MitoGraph data (obtained for each cell via MitoGraph software) and creates several *.png* and *.csv* files. The *.png* files are used to visually inspect the distributions of data, whereas the *.csv* files are used in automatic data filtering and storing the final output.

9. *dataset_merge_mitograph.R*  
Merges experimental replicates of the MitoGraph data generated by the above script. Additionally used to produce violin plots, compute summary metrics, and perform statistical analyses.

10. *dataset_merge_MFIHeatmap_LLPS.R*  
Parses, formats, and merges experimental replicates of *in vitro* phase separation data. Additionally used to produce violin plots, compute summary metrics, and perform statistical analyses.


## Other scripts

*to redistribute images and provide instructions for batch scheduling MitoGraph*

1. *dirDistrib.bat*  
A simple batch script used to evenly distribute mitochondrial fluorescence images into numbered directories. Can only be run from the command line and requires arguments (see remarks in script). Organizing the images in this way may not be strictly necessary to run MitoGraph, although it does appear to benefit the batch scheduling of jobs when performed on a high-performance computing cluster.

2. *mitographrun_20250924_slurm.sh*  
This shell script file is only useful for scheduling the processing of mitochondrial fluorescence images on a high-performance computing cluster. Running MitoGraph locally or on different server infrastructure will require unique setup. 
