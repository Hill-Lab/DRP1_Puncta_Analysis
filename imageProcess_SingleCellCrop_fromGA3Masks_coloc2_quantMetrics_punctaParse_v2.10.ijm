//collect timing metrics
getDateAndTime(year, month, dayOfWeek, dayOfMonth, hour, minute, second, msec);

//minimum version of ImageJ required for macro use
requires("1.53n");

close("ROI Manager");
close("Log");
close("*");
run("Close All");

//speed up macro speed by not displaying windows
setBatchMode(true);

setPasteMode("Copy");

//enable Bio-Formats Macro Extension commands
run("Bio-Formats Macro Extensions");

//select root folder, check for Original_images and GA3_masks folders
//the root folder MUST contain a directory named "Original_images"
//the following is an example of the naming convention for raw image files: "_Wells-B4_Points-006.nd2"
rootFolder = getDirectory("Choose a Directory");
if (!File.isDirectory(rootFolder + "Original_images\\")) {
  print("No \"Original_images\" folder exists within " + rootFolder + ".\nTerminating macro execution.");
  exit;
}
originalImagesFolder = rootFolder + "Original_images\\";

//the root folder MUST contain a directory named "GA3_masks"
//the following is an example of the naming convention for GA3 mask files: "Mask405-DIC_3DColoc488-640_000__Wells-B4_Points-006.nd2"
if (!File.isDirectory(rootFolder + "GA3_masks\\")) {
  print("No \"GA3_masks\" folder exists within " + rootFolder + ".\nTerminating macro execution.");
  exit;
}
masksFolder = rootFolder + "GA3_masks\\";

//create output folders for storing ROI data and cropped cell images
roiFolder = rootFolder + "ROIs\\";
File.makeDirectory(roiFolder);

croppedCellsFolder = rootFolder + "croppedCells\\";
File.makeDirectory(croppedCellsFolder);

//store file list paths, verify they contain images, and automatically detect image file types
originalImagesFileList = getFileList(originalImagesFolder);
originalImagesFileListLength = originalImagesFileList.length;
if (originalImagesFileListLength < 1) {
  print("No images found within " + originalImagesFolder + ".\nTerminating macro execution.");
  exit;
}

//ensure images contain a valid file extension (any additional file types supported by Bio-Formats can be easily added to this list)
imageFileType = split(originalImagesFileList[0], ".");
imageFileType = trim("." + imageFileType[1]);
if (imageFileType != ".nd2" && imageFileType != ".lsm" && imageFileType != ".tif" && imageFileType != ".tiff")
  imageFileType = manualFileTypeSelection();

//store file list paths, verify they contain masks, and automatically detect mask file types
masksFileList = getFileList(masksFolder);
masksFileListLength = masksFileList.length;
if (masksFileListLength < 1) {
  print("No masks found within " + masksFolder + ".\nTerminating macro execution.");
  exit;
}

//ensure GA3 masks contain a valid file extension (any additional file types supported by Bio-Formats can be easily added to this list)
maskFileType = split(masksFileList[0], ".");
maskFileType = trim("." + maskFileType[1]);
if (maskFileType != ".nd2" && maskFileType != ".lsm" && maskFileType != ".tif" && maskFileType != ".tiff")
  maskFileType = manualFileTypeSelection();

//define variables
paramInput = false;
channelInfo = newArray("");
channelSelected = newArray("");
quantAnalysisSelected = newArray("");
mitoMaskingSelected = newArray("");
colocAnalysisSelected = newArray("");
punctaParseSelected = newArray("");
croppedChannels = newArray("");
maskChannelInfo = "";
maskChannelPos = -1;
numSelectedChannels = 0;
channelOffset = 0;
colocOffset = 0;
saveCroppedCells = false;
maxIPStack = false;
quantAnalysis = false;
mitoMasking = false;
colocAnalysis = false;
punctaParse = false;
punctaParse_global = false;
punctaParse_masks = false;

keyList = "";
keysMapped = 0;
masksMapped = 0;
totalROIs = 0;
roisProcessed = 0;

currentROI = 0;
firstROIStartTime = 0;
currentROITime = 0;
elapsedTimeOutput = 0;
estimatedTimeOutput = 0;
avgROITime = 0;

failedRegexMatch = "";
failedKeyMaps = "";
duplicateKeyMaps = "";
failedMaskMaps = "";
failedROIs = "";

run("ROI Manager...");
roiManager("reset");

//create window for displaying macro progress
progressBar = "[Progress]";
run("Text Window...", "name="+ progressBar +" width=37 height=10 monospaced");
runLog = "[Run Log]";
run("Text Window...", "name="+ runLog +" width=90 height=10");

//iterate through Original_images and map file paths to unique identifiers
print(runLog, "Mapping images.");
for (i = 0; i < originalImagesFileListLength; i++) {  
  originalImageFileName = originalImagesFileList[i];
  //handle user input for which channels to process
  if (endsWith(originalImageFileName, imageFileType) && i == 0) {
    originalImageFilePath = originalImagesFolder + originalImageFileName;
    run("Bio-Formats Importer", "open=[" + originalImageFilePath + "] color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT use_virtual_stack");
    getDimensions(origWidth, origHeight, origChannels, origSlices, origFrames);
    getPixelSize(pixelUnit, pixelWidth, pixelHeight);
    run("Set Scale...", "distance=[" + (1/pixelWidth) + "] known=1 unit=[" + pixelUnit + "] global");
    quantAnalysisSelected = newArray(origChannels); 

    //enable user input for various identifiers and runtime parameters
    if (!paramInput) {
      for (j = 0; j < origChannels; j++) channelInfo[j] = "Channel " + (j+1);      
      Dialog.createNonBlocking("Macro Parameters");
      Dialog.setInsets(0,13,0);
      Dialog.addMessage("Select and Identify Channels", 12);
      for (j = 0; j < origChannels; j++) {
        Dialog.setInsets(0,30,0);
        Dialog.addCheckbox("Crop " + toLowerCase(channelInfo[j]) + "?", false);          
        Dialog.addString(channelInfo[j] + " info:", channelInfo[j], 10);        
      }
      Dialog.addString("Home focus slice:", 11, 4);
      Dialog.setInsets(0,2,0);
      Dialog.addCheckbox("Save individual cropped cells?", false);
      Dialog.setInsets(0,2,0);
      Dialog.addCheckbox("Generate single cell MaxIP stack?", false);
      Dialog.setInsets(0,2,0);
      Dialog.addCheckbox("Collect whole field quantification metrics?", false);      
      Dialog.setInsets(0,2,0);
      Dialog.addCheckbox("Implement mitochondrial masking quantification?", false);      
      Dialog.setInsets(0,2,0);
      Dialog.addCheckbox("Perform colocalization analysis?", false);      
      Dialog.setInsets(0,2,0);
      Dialog.addCheckbox("Run advanced puncta parsing?", false);      
      Dialog.show();    

      for (j = 0; j < origChannels; j++) {
        channelSelected[j] = Dialog.getCheckbox();
        channelInfo[j] = Dialog.getString();
        if (channelSelected[j]) numSelectedChannels++;        
      }
      homeFocusSlice = Dialog.getString();
      saveCroppedCells = Dialog.getCheckbox();
      maxIPStack = Dialog.getCheckbox();
      quantAnalysis = Dialog.getCheckbox();
      mitoMasking = Dialog.getCheckbox();
      colocAnalysis = Dialog.getCheckbox();
      punctaParse = Dialog.getCheckbox();        
    }
    close();

    //user option to collect field metrics (ImageJ measure function) per cell
    if (quantAnalysis && numSelectedChannels > 0) {
      Dialog.createNonBlocking("Quantification Analysis Parameters");
      Dialog.setInsets(0,13,0);
      Dialog.addMessage("Select channels to collect quantitative metrics", 12); 
      for (j = 0; j < origChannels; j++) {        
        if (channelSelected[j]) {
          Dialog.setInsets(0,30,0);
          Dialog.addCheckbox(channelInfo[j], false);
        }
      }
      Dialog.show();
      
      for (j = 0; j < origChannels; j++) {
        if (channelSelected[j]) {          
          quantAnalysisSelected[j] = Dialog.getCheckbox();
        }  else {
          quantAnalysisSelected[j] = 0;
        }
      }  
    }    

    //user option to create masks of mitochondrial immunofluorescence
    if (mitoMasking && numSelectedChannels > 1) {
      for (j = 0; j < origChannels; j++) {
        if (channelSelected[j]) {
          croppedChannels[channelOffset] = channelInfo[j];
          channelOffset++;
        }
      }
      Dialog.createNonBlocking("Mitochondrial Masking Parameters");
      Dialog.setInsets(0,13,0);
      Dialog.addMessage("Select a channel to be used for mitochondrial masking", 12);     
      Dialog.addChoice("Channel:", croppedChannels);
      Dialog.show();
      maskChannelInfo = Dialog.getChoice();
      for (j = 0; j < origChannels; j++) {
        if (channelInfo[j] == maskChannelInfo)
          maskChannelPos = j;
      }

      Dialog.createNonBlocking("Mitochondrial Masking Parameters");
      Dialog.setInsets(0,13,0);
      Dialog.addMessage("Select channel(s) to be used in mask arithmetic", 12);   //should NOT be the same channel used to generate the mitochondrial masks above
      for (j = 0; j < origChannels; j++) {
        if (channelSelected[j] && j != maskChannelPos) {
          Dialog.setInsets(0,30,0);
          Dialog.addCheckbox(channelInfo[j], false);
        }
      }
      Dialog.show();

      for (j = 0; j < origChannels; j++) {
        if (channelSelected[j] && j != maskChannelPos) {          
          mitoMaskingSelected[j] = Dialog.getCheckbox();
        } else {
          mitoMaskingSelected[j] = 0;
        }
      } 
    }

    //user option to collect colocalization data
    if (colocAnalysis && numSelectedChannels > 1) {      
      Dialog.createNonBlocking("Colocalization Analysis Parameters");
      Dialog.setInsets(0,13,0);
      Dialog.addMessage("Select channel permutations for colocalization", 12);
      for (j = 0; j < origChannels-1; j++) {
        for (k = j+1; k < origChannels; k++) {
          if (channelSelected[j] && channelSelected[k]) {
            Dialog.setInsets(0,30,0);
            Dialog.addCheckbox(channelInfo[j] + " and " + channelInfo[k], false);            
          }
        }
      }
      Dialog.show();
      
      for (j = 0; j < origChannels-1; j++) {
        for (k = j+1; k < origChannels; k++) {
          if (channelSelected[j] && channelSelected[k]) {
            if (Dialog.getCheckbox()) {
              colocAnalysisSelected[colocOffset] = toString(j) + "-" + toString(k);
              colocOffset++;
            }
          }
        }
      }
    }

    //user option to run advanced puncta parsing via 3D segmentation through the 3D ImageJ Suite plugin (https://imagej.net/plugins/3d-imagej-suite/)
    if (punctaParse && numSelectedChannels > 0) {
      Dialog.createNonBlocking("Puncta Parse Parameters");
      Dialog.setInsets(0,13,0);
      Dialog.addMessage("Select channels for advanced puncta parsing", 12);
      for (j = 0; j < origChannels; j++) {
        if (channelSelected[j]) {
          Dialog.setInsets(0,30,0);
          Dialog.addCheckbox(channelInfo[j], false);
        }
      }
      Dialog.show();

      for (j = 0; j < origChannels; j++) {
        if (channelSelected[j]) {
          punctaParseSelected[j] = Dialog.getCheckbox();
        } else {
          punctaParseSelected[j] = 0;
        }
      }
      Dialog.createNonBlocking("Puncta Parse Parameters");
      Dialog.setInsets(0,13,0);
      Dialog.addMessage("Select methods for advanced puncta parsing", 12);
      Dialog.addCheckbox("Global parsing", false);    //global parsing collects data for an entire cell, agnostic to subcellular localization
      Dialog.addCheckbox("Mask parsing", false);    //mask parsing collects data for an entire cell, splitting the population into masked populations based on their subcellular localization
      Dialog.show();

      punctaParse_global = Dialog.getCheckbox();
      punctaParse_masks = Dialog.getCheckbox();
    }
  }

  //iterate through images and map file names following naming convention (note the padded zeroes where relevant)
  //this code section ensures that a raw image file and its corresponding GA3 mask are indexed together and will be opened simultaneously during processing
  //the following is an example of the naming convention for raw image files: "_Wells-B4_Points-006.nd2"
  //the following is an example of the naming convention for GA3 mask files: "Mask405-DIC_3DColoc488-640_000__Wells-B4_Points-006.nd2"
  print(runLog, "\\Update:Mapping image " + (i+1) + " of " + originalImagesFileListLength + ".");   
  if (matches(originalImageFileName,"^.*retake_[Ww]ells-[a-zA-Z][0-9]{1,2}_[Pp]oints-[0-9]{3}\." + replace(imageFileType, ".", "") + "$")) {
    key = "r" + fileNameSplit(originalImageFileName);
    keyList = keyList + key + "~~~";
      keysMapped++;
      List.set(key, originalImagesFolder + originalImageFileName + "~~~");
  }

  else if (matches(originalImageFileName,"^.*_[Ww]ells-[a-zA-Z][0-9]{1,2}_[Pp]oints-[0-9]{3}\." + replace(imageFileType, ".", "") + "$")) {
    key = fileNameSplit(originalImageFileName);
    keyList = keyList + key + "~~~";
      keysMapped++;
      List.set(key, originalImagesFolder + originalImageFileName + "~~~");
  }
  else failedRegexMatch = failedRegexMatch + originalImageFileName + "~~~";

}
print(runLog, "\\Update:Completed mapping images.");

//create quantification metric output folders if selected
if (quantAnalysis) {
  run("Set Measurements...", "area mean standard min perimeter integrated median area_fraction stack limit display redirect=None decimal=4");
  quant_allZ = rootFolder + "quant_metrics_allZ\\";
  File.makeDirectory(quant_allZ);
  if (maxIPStack) {
    quant_maxIP = rootFolder + "quant_metrics_maxIP\\";
    File.makeDirectory(quant_maxIP);
  }
  quant_puncta_allZ = rootFolder + "quant_puncta_allZ\\";
  File.makeDirectory(quant_puncta_allZ);
  if (maxIPStack) {
    quant_puncta_maxIP = rootFolder + "quant_puncta_maxIP\\";
    File.makeDirectory(quant_puncta_maxIP);
  }
}

//create mitochondrial masking arithmetic output folders if selected
if (mitoMasking) {
  mask_quant = rootFolder + "mask_quant\\";
  File.makeDirectory(mask_quant);
}

//create colocalization analysis output folders if selected
if (colocAnalysis) {
  coloc_allZ = rootFolder + "coloc2_allZ\\";
  File.makeDirectory(coloc_allZ);
  if (maxIPStack) {
    coloc_maxIP = rootFolder + "coloc2_maxIP\\";
    File.makeDirectory(coloc_maxIP);
  }
}

//create puncta parse analysis output folders if selected
if (punctaParse && punctaParse_global) {
  punctaParse_3D_global = rootFolder + "punctaParse_global\\";
  File.makeDirectory(punctaParse_3D_global);
}
if (punctaParse && punctaParse_masks) {
  punctaParse_3D_masks = rootFolder + "punctaParse_masks\\";
  File.makeDirectory(punctaParse_3D_masks);
}

//iterate through GA3_masks and map file paths using previously set up unique identifiers (see naming conventions above at lines 301 and 302)
print(runLog, "Mapping masks and ROIs.");
for (i = 0; i < masksFileListLength; i++) {
  maskFileName = masksFileList[i];
  print(runLog, "\\Update:Mapping mask " + (i+1) + " of " + masksFileListLength + "."); 
  if (matches(maskFileName,"^Mask.*retake_[Ww]ells-[a-zA-Z][0-9]{1,2}_[Pp]oints-[0-9]{3}\." + replace(maskFileType, ".", "") + "$")) {
    key = "r" + fileNameSplit(maskFileName);
    if (List.get(key) != "") {
      masksMapped++;

      numROIs = detectROIs(masksFolder + maskFileName);
      if (numROIs == 0) {
        failedROIs = failedROIs + maskFileName + "~~~";
      }
      else {
        List.set(key, List.get(key) + roiFolder + key + ".zip");
        roiManager("save", roiFolder + key + ".zip");
        totalROIs = totalROIs + numROIs;
      }        
    }
    else failedMaskMaps = failedMaskMaps + maskFileName + "~~~";
  }

  else if (matches(maskFileName,"^Mask.*_[Ww]ells-[a-zA-Z][0-9]{1,2}_[Pp]oints-[0-9]{3}\." + replace(maskFileType, ".", "") + "$")) {
    key = fileNameSplit(maskFileName);
    if (List.get(key) != "") {
      masksMapped++;

      numROIs = detectROIs(masksFolder + maskFileName);
      if (numROIs == 0) {
        failedROIs = failedROIs + maskFileName + "~~~";
      }
      else {
        List.set(key, List.get(key) + roiFolder + key + ".zip");
        roiManager("save", roiFolder + key + ".zip");
        totalROIs = totalROIs + numROIs;
      }    
    }
    else failedMaskMaps = failedMaskMaps + maskFileName + "~~~";
  }
  else failedRegexMatch = failedRegexMatch + maskFileName + "~~~";
}
print(runLog, "\\Update:Completed mapping masks and ROIs.");

//store a unique variable for the combined MaxIP stack, if selected by the user
if (maxIPStack) {
  newImage("maxIP_stack", "16-bit color-mode", origWidth, origHeight, numSelectedChannels, totalROIs, 1);
  _maxIP = getImageID();
}

//generate an array of keys mapped to image and mask file paths
keyList = split(keyList, "~~~");

//iterate through keys and begin handling image and mask files
for (i = 0; i < keysMapped; i ++) {
  //extract mapped file paths
  key = keyList[i]; 
  keyResult = split(List.get(key), "~~~");
  
  //proceed with ROI cropping only if an image was successfully paired with a mask
  if (keyResult.length == 2) {
    originalImageFilePath = keyResult[0];   
    roiFilePath = keyResult[1];    

    run("ROI Manager...");
    roiManager("reset");

    print(runLog, "Opening image and ROIs mapped to " + key + ".");

    //open image and mask files and store unique ID values for toggling between active windows
    run("Bio-Formats Importer", "open=[" + originalImageFilePath + "] color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT use_virtual_stack");
    _original = getImageID();

    //store some basic image information for processing
    getDimensions(origWidth, origHeight, origChannels, origSlices, origFrames);

    //open ROI .zip file    
    roiManager("Open", roiFilePath);
    RoiManager.associateROIsWithSlices(false);
    numROIs = roiManager("count");

    //iterate through each ROI in mask for cropping
    for (j = 0; j < numROIs; j++) {
      //display overall progress and estimated time remaining
      if (currentROI==0) {
          firstROIStartTime = getTime();
          elapsedTimeOutput = "0 seconds";
          estimatedTimeOutput = "Calculating...";
      } else {
          currentROITime = getTime();
          elapsedTimeOutput = timeConvert((currentROITime-firstROIStartTime)/1000);
          avgROITime = ((currentROITime-firstROIStartTime)/1000)/currentROI;
          estimatedTimeOutput = timeConvert(avgROITime*(totalROIs-currentROI));
      }

      print(progressBar, "\\Update:"+currentROI+"/"+totalROIs+" ("+round((currentROI*100)/totalROIs)+"%)\n"+getBar(round(currentROI/totalROIs*100), 100)+"\n\nElapsed time:\n"+elapsedTimeOutput+"\n\nEstimated time remaining:\n"+estimatedTimeOutput);
      currentROI++;      
        
      roiManager("Deselect");
      selectImage(_original);      
      roiManager("Select", j);      

      currentChannel = 0;

      _roi = newArray();
      _roi_title = newArray();

      roiInfo = newArray();

      if (maxIPStack) {
        _roi_maxIP = newArray();
        _roi_maxIP_title = newArray();
      }
      
      if (mitoMasking) {
        _roi_mask_mito = newArray();
        _roi_mask_mito_title = newArray();
        _roi_mask_cyto = newArray();
        _roi_mask_cyto_title = newArray();
      }

      //iterate through user-selected image channels
      for (k = 0; k < origChannels; k++) {          
        if (channelSelected[k]) {
          //create a new image for cropping based on the ROI bounding box and total number of slices in image 
          newImage("ROI_" + (j+1) + "_" + channelInfo[k] + "_crop", "16-bit Black", origWidth, origHeight, origSlices);
          _roi[k] = getImageID();
          _roi_title[k] = getTitle();
          
          croppedCellsSubfolder = croppedCellsFolder + channelInfo[k] + "\\";
          if (!File.isDirectory(croppedCellsSubfolder)) File.makeDirectory(croppedCellsSubfolder);

          //for estimating noise distribution around ROI per slice
          max_ai = 0;

          currentChannel++;

          //iterate through image slices 
          for (l = 0; l < origSlices; l++) {
            print(runLog, "\\Update:Cropping ROI " + (j+1) + " of " + numROIs + " (channel " + channelInfo[k] + "; slice " + (l+1) + " of " + origSlices + ") mapped to "+ key + ".");

            //toggle original image and set current slice, gather statistics, and copy selection
            selectImage(_original);
            Stack.setPosition(k+1, l+1, 1);           
            getStatistics(area, mean, min, max, std);          
            selectImage(_original);
            roiManager("Select", j);  
            run("Copy");

            //toggle new image and paste selection
            selectImage(_roi[k]);
            setSlice(l+1);
            selectImage(_roi[k]);            
            run("Paste");

            //calculate noise generation per slice based on mean intensity
            getStatistics(area, mean, min, max, std);
            if (mean>max_ai) {
              max_ai = mean;
              slice_max_ai = l;
            }            
          }

          //if enabled, save individual cropped cell          
          if (saveCroppedCells) {
            selectImage(_roi[k]);
            save(croppedCellsSubfolder + key + "_" + IJ.pad(j+1,2) + ".tif");          
          }

          //if selected, run maximum intensity projection on cropped cell and add to MaxIP stack file
          if (maxIPStack) {            
            print(runLog, "\\Update:Z-projecting ROI " + (j+1) + " of " + numROIs + " (channel " + channelInfo[k] + ") mapped to " + key + ".");
            selectImage(_roi[k]);
            roiManager("Deselect");
            run("Z Project...", "projection=[Max Intensity]");
            _roi_maxIP[k] = getImageID();
            _roi_maxIP_title[k] = getTitle();
            selectImage(_roi_maxIP[k]);
            run("Copy");              
            selectImage(_maxIP);
            Stack.setPosition(currentChannel, currentROI, 1);
            run("Paste");
            setMetadata("Label", key + "_" + IJ.pad(j+1,2));  
          }

          //if enabled, gather and save quantification analysis measurements
          if (quantAnalysisSelected[k]) {
            print(runLog, "\\Update:Gathering quantitative field metrics for ROI " + (j+1) + " of " + numROIs + " (channel " + channelInfo[k] + "; all Z-slices) mapped to " + key + ".");
            selectImage(_roi[k]);
            roiInfo = centerROI(j, origWidth, origHeight);

            selectImage(_roi[k]);
            roiManager("Select", j);                            
            run("Enhance Contrast", "saturated=0.35");
            setThreshold(150, 65535, "raw");
            run("Measure");
            selectWindow("Results");
            saveAs("Results", quant_allZ + key + "_allZ_" + replace(_roi_title[k], ".tif", "") + ".csv");
            run("Clear Results");                            
            selectImage(_roi[k]);  
            run("Analyze Particles...", "size=0.02-1.0 circularity=0.80-1.00 display");
            selectWindow("Results");
            saveAs("Results", quant_puncta_allZ + key + "_puncta_allZ_" + replace(_roi_title[k], ".tif", "") + ".csv");
            run("Clear Results");

            if (maxIPStack) {
              print(runLog, "\\Update:Gathering quantitative field metrics for ROI " + (j+1) + " of " + numROIs + " (channel " + channelInfo[k] + "; MaxIP) mapped to " + key + ".");
              
              selectImage(_roi_maxIP[k]);
              roiManager("Select", j);                            
              run("Enhance Contrast", "saturated=0.35");
              setThreshold(150, 65535, "raw");
              run("Measure");
              selectWindow("Results");
              saveAs("Results", quant_maxIP + key + "_maxIP_" + replace(_roi_maxIP_title[k], ".tif", "") + ".csv");
              run("Clear Results");                            
              selectImage(_roi_maxIP[k]);  
              run("Analyze Particles...", "size=0.02-1.0 circularity=0.80-1.00 display");
              selectWindow("Results");
              saveAs("Results", quant_puncta_maxIP + key + "_puncta_maxIP_" + replace(_roi_maxIP_title[k], ".tif", "") + ".csv");
              run("Clear Results");                           
            }

            selectImage(_roi[k]);
            resetROI(j, parseInt(roiInfo[0]), parseInt(roiInfo[1])); 
          }

          //if enabled, create masks of the selected channel for downstream arithmetic
          if (mitoMasking && (k == maskChannelPos)) {           

            print(runLog, "\\Update:Generating Mito- and Cyto- masks for ROI " + (j+1) + " of " + numROIs + " (channel " + maskChannelInfo + ") mapped to " + key + ".");
            
            selectImage(_roi[k]);
            roiInfo = centerROI(j, origWidth, origHeight);
            
            selectImage(_roi[k]);
            run("Duplicate...", "title=[mitoMask_" + j + "] ignore duplicate");
            _temp_mitoMask = getImageID();
            setSlice(homeFocusSlice);
            run("Enhance Contrast", "saturated=0.35");
            roiManager("Select", j);
            setThreshold(600, 65535, "raw");
            run("Convert to Mask", "background=Dark list create");
            //run("Dilate", "stack");                                                           //code snippet to dilate the mask; enabling dilate and erode calls will slightly prune puncta considered in both masks
            //save(punctaParse_3D_masks + "mitoMask_" + key + "_" + IJ.pad(j+1,2) + ".tif");    //code snippet used to save and visually validate masks, if desired
            _roi_mask_mito[k] = getImageID();
            _roi_mask_mito_title[k] = getTitle();

            selectImage(_roi[k]);                       
            run("Duplicate...", "title=[cytoMask_" + j + "] ignore duplicate");
            _temp_cytoMask = getImageID();
            setSlice(homeFocusSlice);
            run("Enhance Contrast", "saturated=0.35");
            roiManager("Select", j);
            setThreshold(0, 600, "raw");
            run("Convert to Mask", "background=Dark list create");
            //run("Erode", "stack");                                                            //code snippet to erode the mask; enabling dilate and erode calls will slightly prune puncta considered in both masks
            //save(punctaParse_3D_masks + "cytoMask_" + key + "_" + IJ.pad(j+1,2) + ".tif");    //code snippet used to save and visually validate masks, if desired
            _roi_mask_cyto[k] = getImageID();
            _roi_mask_cyto_title[k] = getTitle();

            selectImage(_roi[k]);
            resetROI(j, parseInt(roiInfo[0]), parseInt(roiInfo[1]));

            selectImage(_temp_mitoMask);
            close();
            selectImage(_temp_cytoMask);
            close();
          }
        }
      }

      //if enabled, gather and save mitochondrial mask arithmetic data (and potentially run advanced puncta parsing on each masked image)
      if (mitoMasking) {
        for (k = 0; k < mitoMaskingSelected.length; k++) {
          if ((channelSelected[k]) && (mitoMaskingSelected[k]) && (k != maskChannelPos)) {
            print(runLog, "\\Update:Performing mask arithmetic for ROI " + (j+1) + " of " + numROIs + " (channel " + channelInfo[k] + ") mapped to " + key + ".");
            selectImage(_roi[k]);
            roiInfo = centerROI(j, origWidth, origHeight);           

            selectImage(_roi[k]);             
            run("Duplicate...", "title=cropped_duplicate ignore duplicate");                        

            //perform mask arithmetic on the mitochondrial mask
            selectImage(_roi_mask_mito[maskChannelPos]);
            roiManager("Select", j); 
            imageCalculator("Divide create 32-bit stack", "cropped_duplicate", _roi_mask_mito[maskChannelPos]);
            rename("mitoMask_div");
            _mitoDiv = getImageID();
            imageCalculator("Multiply create 32-bit stack", _mitoDiv, _roi_mask_mito[maskChannelPos]);
            rename("mitoMask_mult");
            _mitoMult = getImageID();
            
            roiManager("Select", j);
            run("Duplicate...", "title=mitoMask_mult_cropped duplicate");
            _mitoMult_cropped = getImageID();
            _mitoMult_cropped_title = getTitle();

            selectImage(_mitoMult);
            setSlice(homeFocusSlice); 
            run("Enhance Contrast", "saturated=0.35");            
            roiManager("Select", j);                       
            setThreshold(150, 65535);
            run("Measure Stack...");            

            //perform mask arithmetic on the cytosolic mask
            selectImage(_roi_mask_cyto[maskChannelPos]);
            roiManager("Select", j);
            imageCalculator("Divide create 32-bit stack", "cropped_duplicate", _roi_mask_cyto[maskChannelPos]);
            _cytoDiv = getImageID();
            rename("cytoMask_div");
            imageCalculator("Multiply create 32-bit stack", _cytoDiv, _roi_mask_cyto[maskChannelPos]);
            rename("cytoMask_mult");
            _cytoMult = getImageID();
            
            roiManager("Select", j);
            run("Duplicate...", "title=cytoMask_mult_cropped duplicate");
            _cytoMult_cropped = getImageID();
            _cytoMult_cropped_title = getTitle();

            selectImage(_cytoMult);
            setSlice(homeFocusSlice); 
            run("Enhance Contrast", "saturated=0.35");           
            roiManager("Select", j);                       
            setThreshold(150, 65535);
            run("Measure Stack...");            

            //save mask arithmetic results
            selectWindow("Results");
            saveAs("Results", mask_quant + key + "_" + IJ.pad(j+1,2) + "_maskQuant.csv");            
            run("Clear Results");

            //main function call to advanced puncta parsing, if selected
            //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            if (punctaParse_masks && punctaParseSelected[k]) {
              print(runLog, "\\Update:Running advanced puncta parsing (MSER_STEP) using mito mask on ROI " + (j+1) + " of " + numROIs + " (channel " + channelInfo[k] + ") mapped to " + key + ".");
              advancedPunctaParse(_mitoMult_cropped_title, punctaParse_3D_masks, key, "MitoMask", j, true);
              print(runLog, "\\Update:Running advanced puncta parsing (MSER_STEP) using cyto mask on ROI " + (j+1) + " of " + numROIs + " (channel " + channelInfo[k] + ") mapped to " + key + ".");
              advancedPunctaParse(_cytoMult_cropped_title, punctaParse_3D_masks, key, "CytoMask", j, true);
            }/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

            selectImage(_roi[k]);
            resetROI(j, parseInt(roiInfo[0]), parseInt(roiInfo[1])); 

            selectImage("cropped_duplicate");            
            close();
            selectImage(_mitoDiv);
            close();
            selectImage(_mitoMult);
            close();
            selectImage(_mitoMult_cropped);
            close();
            selectImage(_cytoDiv);
            close();
            selectImage(_cytoMult);
            close();  
            selectImage(_cytoMult_cropped);
            close();  
          }
        }
      }

      //if enabled, gather and save colocalization metrics between selected channel pairings
      if (colocAnalysis && colocOffset > 0) {
        for (k = 0; k < colocOffset; k++) {          
          colocChannels = split(colocAnalysisSelected[k], "-");
          colocCh1 = colocChannels[0];
          colocCh2 = colocChannels[1];

          print(runLog, "\\Update:Performing colocalization on ROI " + (j+1) + " of " + numROIs + " (channels " + channelInfo[colocCh1] + " and " + channelInfo[colocCh2] + "; all Z-slices) mapped to " + key + ".");
          selectImage(_roi[colocCh1]);
          roiInfo = centerROI(j, origWidth, origHeight);

          selectImage(_roi[colocCh1]);
          roiManager("Select", j);          

          run("Coloc 2", "channel_1=[" + _roi_title[colocCh1] + "] channel_2=[" + _roi_title[colocCh2] + "] roi_or_mask=[ROI(s) in channel 1] threshold_regression=Bisection psf=3 manders'_correlation costes_randomisations=10");
          selectWindow("Log");
          saveAs("Text", coloc_allZ + "\\" + key + "_allZ_" + replace(_roi_title[colocCh1], ".tif", "") + "_versus_" + replace(_roi_title[colocCh2], ".tif", "") + ".txt");
          selectWindow("Log");
          print("\\Clear");
          run("IJ Robot", "order=Left_Click x_point=569 y_point=16 delay=5 keypress=[]");

          if (maxIPStack) {
            print(runLog, "\\Update:Performing colocalization on ROI " + (j+1) + " of " + numROIs + " (channels " + channelInfo[colocCh1] + " and " + channelInfo[colocCh2] + "; MaxIP) mapped to " + key + ".");
            selectImage(_roi_maxIP[colocCh1]);
            roiManager("Select", j);  
            run("Coloc 2", "channel_1=[" + _roi_maxIP_title[colocCh1] + "] channel_2=[" + _roi_maxIP_title[colocCh2] + "] roi_or_mask=[ROI(s) in channel 1] threshold_regression=Bisection psf=3 manders'_correlation costes_randomisations=10");
            selectWindow("Log");
            saveAs("Text", coloc_maxIP + "\\" + key + "_maxIP" + replace(_roi_maxIP_title[colocCh1], ".tif", "") + "_versus_" + replace(_roi_maxIP_title[colocCh2], ".tif", "") + ".txt");
            selectWindow("Log");
            print("\\Clear");
            run("IJ Robot", "order=Left_Click x_point=569 y_point=16 delay=5 keypress=[]");
          }

          selectImage(_roi[colocCh1]);
          resetROI(j, parseInt(roiInfo[0]), parseInt(roiInfo[1]));                    
        }
      }

      //if enabled run advanced puncta parsing globally
      if (punctaParse_global) {
        for (k = 0; k < punctaParseSelected.length; k++) {
          if (punctaParseSelected[k]) {
            selectImage(_roi[k]);
            roiInfo = centerROI(j, origWidth, origHeight);

            selectImage(_roi[k]);
            roiManager("Select", j); 
            run("Duplicate...", "title=cropped_duplicate duplicate");           

            //main function call to advanced puncta parsing
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
            print(runLog, "\\Update:Running advanced puncta parsing (MSER_STEP) globally on ROI " + (j+1) + " of " + numROIs + " (channel " + channelInfo[k] + ") mapped to " + key + ".");
            advancedPunctaParse("cropped_duplicate", punctaParse_3D_global, key, "Global", j, true);
            ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

            selectImage(_roi[k]);
            resetROI(j, parseInt(roiInfo[0]), parseInt(roiInfo[1]));   

            selectImage("cropped_duplicate");
            close();
          }
        }        
      }

      //close all open images pertaining to the current cell/ROI
      for (k = 0; k < _roi.length; k++) {
      	if (isOpen(_roi[k])) {
          selectImage(_roi[k]);
          close();
        }
        if (isOpen(_roi_title[k])) {
          selectImage(_roi_title[k]);
          close();
        }        
      }

      if (maxIPStack) {
        for (k = 0; k < _roi_maxIP.length; k++) {
          if (isOpen(_roi_maxIP[k])) {
            selectImage(_roi_maxIP[k]);
            close();
          }
          if (isOpen(_roi_maxIP_title[k])) {
            selectImage(_roi_maxIP_title[k]);
            close();
          }        
        } 
      }

      if (mitoMasking) {
        for (k = 0; k < _roi_mask_mito.length; k++) {
          if (isOpen(_roi_mask_mito[k])) {
            selectImage(_roi_mask_mito[k]);
            close();
          }
          if (isOpen(_roi_mask_mito_title[k])) {
            selectImage(_roi_mask_mito_title[k]);
            close();
          }        
        }
        for (k = 0; k < _roi_mask_cyto.length; k++) {
          if (isOpen(_roi_mask_cyto[k])) {
            selectImage(_roi_mask_cyto[k]);
            close();
          }
          if (isOpen(_roi_mask_cyto_title[k])) {
            selectImage(_roi_mask_cyto_title[k]);
            close();
          } 
        } 
      }

      run("Select None");                 
      resetMinAndMax();
    }   

    //close the original image after all its ROIs were processed individually    
    selectImage(_original);
    close();

    if (numROIs == 1) print(runLog, "\\Update:Completed cropping " + numROIs + "  ROI mapped to " + key + ".\n");
    else print(runLog, "\\Update:Completed cropping " + numROIs + " ROIs mapped to " + key + ".\n");
    roisProcessed = roisProcessed + numROIs;
  }
  else if (keyResult.length == 1) failedKeyMaps = failedKeyMaps + key + "~~~";
  else (if (!duplicateKeyMaps.contains(key) duplicateKeyMaps = duplicateKeyMaps + key + "~~~");
}

//if selected, save MaxIP stack of all cropped cells (WARNING: this is a VERY large file when working with large datsets)
if (maxIPStack) {
  print(runLog, "Saving MaxIP stack.");
  selectImage(_maxIP);
  save(rootFolder + "maxIP_stack.tif");
  close(_maxIP);
}

//close runtime progress indicator
print(progressBar, "\\Close");
close("ROI Manager");
close("Log");
run("Close All");
endTime = getTime();
outputText = "\n";

//handle any failed regex matches or mappings
if (roisProcessed == 0) print(runLog, "No ROIs were processed.");
else {
  if (failedRegexMatch == "" && failedKeyMaps == "" && duplicateKeyMaps == "" && failedMaskMaps == "") 
    outputText = outputText + "All mappings were successful and no errors were encountered.\n\n";
  else {
    if (failedRegexMatch != "") {
      failedRegexMatch = split(failedRegexMatch, "~~~");
      outputText = outputText + "The following files could not be matched by regular expression:\n";
      for (i = 0; i < failedRegexMatch.length; i++) outputText = outputText + failedRegexMatch[i] + "\n";
      outputText = outputText + "\n";         
    }
    if (failedKeyMaps != "") {
      failedKeyMaps = split(failedKeyMaps, "~~~");
      outputText = outputText + "The following keys were not mapped to a mask:\n";
      for (i = 0; i < failedKeyMaps.length; i++) outputText = outputText + failedKeyMaps[i] + "\n";
      outputText = outputText + "\n";
    }
    if (duplicateKeyMaps != "") {
      duplicateKeyMaps = split(duplicateKeyMaps, "~~~");
      outputText = outputText + "The following keys were mapped to more than one mask:\n";
      for (i = 0; i < duplicateKeyMaps.length; i++) outputText = outputText + duplicateKeyMaps[i] + "\n";
      outputText = outputText + "\n";
    }
    if (failedMaskMaps != "") {
      failedMaskMaps = split(failedMaskMaps, "~~~");
      outputText = outputText + "The following masks were not mapped to a key:\n";
      for (i = 0; i < failedMaskMaps.length; i++) outputText = outputText + failedMaskMaps[i] + "\n";
      outputText = outputText + "\n";
    }
    if (failedROIs != "") {
      failedROIs = split(failedROIs, "~~~");
      outputText = outputText + "The following masks had no ROIs detected:\n";
      for (i = 0; i < failedROIs.length; i++) outputText = outputText + failedROIs[i] + "\n";
      outputText = outputText + "\n";
    }
  } 

  //handle some other data to output in log file
  if (numSelectedChannels == 1) channelString = " channel, ";
  else channelString = " channels, ";

  if (origSlices == 1) sliceString = " slice) in ";
  else sliceString = " slices) in ";

  if (roisProcessed == 1) roiString = " ROI (";
  else roiString = " ROIs (";

  if (masksMapped == 1) maskString = " mask.\n";
  else maskString = " masks.\n";

  outputText = outputText + "Finished cropping a total of " + roisProcessed + roiString + numSelectedChannels + channelString + origSlices + sliceString + masksMapped + maskString;
  outputText = outputText + "Total time elapsed: " + timeConvert((endTime - firstROIStartTime)/1000) + ".\n";
  outputText = outputText + "Average time per ROI: " + timeConvert(((endTime - firstROIStartTime)/1000)/totalROIs) + ".\n";
}
print(runLog, outputText);
print(runLog, "All ImageJ windows may now be safely closed.");

//generate a log file with macro run information
bioVersion = newArray("");
bioBuild = newArray("");
Ext.getVersionNumber(bioVersion[0]);
Ext.getBuildDate(bioBuild[0]);

logChannels = "";
if (month < 9) month = "0" + toString(month+1);
else month = toString(month+1);
logFileName = "Log_" + toString(year) + month + toString(dayOfMonth) + "_" + toString(hour) + toString(minute) + toString(second) + ".txt";
logFilePath = rootFolder + logFileName;
logFile = File.open(logFilePath);

print(logFile, "ImageJ version: " + IJ.getFullVersion() + "\n");
print(logFile, "Bio-Formats version: " + bioVersion[0] + "\n");
print(logFile, "Bio-Formats build date: " + bioBuild[0] + "\n\n");
print(logFile, "Number of image files detected: " + originalImagesFileListLength + "\n");
print(logFile, "Number of image files mapped: " + keysMapped + "\n");
print(logFile, "Image file type: " + imageFileType + "\n\n");
print(logFile, "Number of mask files detected: " + masksFileListLength + "\n");
print(logFile, "Number of mask files mapped: " + masksMapped + "\n");
print(logFile, "Mask file type: " + maskFileType + "\n\n");
print(logFile, "Number of ROIs detected: " + totalROIs + "\n");
print(logFile, "Number of ROIs cropped: " + roisProcessed + "\n\n");

for (i = 0; i < origChannels; i++) {
  if (channelSelected[i]) {
    if (logChannels == "") logChannels = "Channel " + (i+1) + " (" + channelInfo[i] + ")";
    else logChannels = logChannels + ", Channel " + (i+1) + " (" + channelInfo[i] + ")";
  }
}

print(logFile, "Selected channels: " + logChannels + "\n");
print(logFile, "Saved individual cropped cells: " + saveCroppedCells + "\n");
print(logFile, "Saved MaxIP stack: " + maxIPStack + "\n");
print(logFile, "Performed quantification analyses: " + quantAnalysis + "\n");
print(logFile, "Performed mitochondrial masking: " + mitoMasking + "\n");
print(logFile, "Performed colocalizaiton analysis: " + colocAnalysis + "\n");
print(logFile, "Performed advanced puncta parsing globally: " + punctaParse_global + "\n");
print(logFile, "Performed advanced puncta parsing with masks: " + punctaParse_masks + "\n");

print(logFile, outputText);
File.close(logFile);
exit;


////////////////////////////////////////////////////////////////////////////////////////////////////


//helper function for advanced puncta parsing
function advancedPunctaParse(ImageTitle, SavePath, Key, Type, ROI_index, Beta) {
  selectImage(ImageTitle);

  //these parameters were hard-coded for simplicity, but could be modified or user-specified if desired
  if (!Beta) run("3D Iterative Thresholding", "min_vol_pix=8 max_vol_pix=500 min_threshold=150 min_contrast=0 criteria_method=MSER threshold_method=STEP segment_results=All value_method=10");
  else run("3D Iterative Thresholding 2 (beta)", "seeds=None min_vol_pix=8 max_vol_pix=500 min_threshold=150 min_contrast=0 criteria_method=MSER threshold_method=STEP segment_results=All value_method=10");
  
  //the "draw" image is the default name of the resultant output from running the 3D Iterative Thresholding function above
  //the resultant output is an image containing several channels with the segmented structures; a majority of the data is contained within channels 1 and 2
  //larger segmented objects are typically found in channels 3 and 4 and are excluded from analyses
  if (isOpen("draw")) {
    selectImage("draw");
    run("Duplicate...", "title=stack1 duplicate channels=1");
    selectImage("draw");
    run("Duplicate...", "title=stack2 duplicate channels=2");
    selectImage("draw");
    close();

    //perform XOR image arithmetic to preserve unique puncta between channels 1 and 2
    imageCalculator("XOR create stack", "stack1","stack2");
    selectImage("Result of stack1");
    rename("XORstack");
    run("Enhance Contrast", "saturated=0.35");
    selectImage("stack1");
    close();
    selectImage("stack2");
    close();

    //perform 3D measurements and save data
    selectImage("XORstack");
    run("3D Intensity Measure", "objects=XORstack signal=[" + ImageTitle + "]");
    selectWindow("Results");
    saveAs("Results", SavePath + Key + "_" + IJ.pad(ROI_index + 1, 2) + "_" + Type + "_3D-Intensity_MSER_STEP.csv");
    run("Clear Results");
    selectImage("XORstack");
    run("3D Volume");
    selectWindow("Results");
    saveAs("Results", SavePath + Key + "_" + IJ.pad(ROI_index + 1, 2) + "_" + Type + "_3D-Volume_MSER_STEP.csv");
    run("Clear Results");
    selectImage("XORstack");
    run("3D Compactness");
    selectWindow("Results");
    saveAs("Results", SavePath + Key + "_" + IJ.pad(ROI_index + 1, 2) + "_" + Type + "_3D-Compactness_MSER_STEP.csv");
    run("Clear Results");
    selectImage("XORstack");
    close();
  }
}

//helper function for manual file type selection if autodetection fails (any additional file types supported by Bio-Formats can be easily added to this list)
function manualFileTypeSelection() {
  Dialog.createNonBlocking("Select File Type");
  Dialog.setInsets(0,5,0);
  Dialog.addMessage("Automatic file type detection failed!\n");
  Dialog.addMessage("Please select the file type of\nthe images to be processed.");
  Dialog.setInsets(0,30,0);
  Dialog.addChoice("File type:", newArray(".nd2", ".lsm", ".tif"));
  Dialog.show();
  return Dialog.getChoice();
}

//helper function for parsing file names for key generation
function fileNameSplit(fileName) {
  trunc = substring(fileName, fileName.indexOf("ells-"));
  dashSplit = split(trunc, "-");
    wellIdentifier = split(dashSplit[1], "_");
    wellIdentifier = toUpperCase(wellIdentifier[0]);
    pointIdentifier = split(dashSplit[2], "\.");
    return wellIdentifier + "-" + pointIdentifier[0];
}

//helper function for generating ROIs from masks
function detectROIs(maskFilePath) {
  roiManager("reset");
  run("Bio-Formats Importer", "open=[" + maskFilePath + "] color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT use_virtual_stack");
  setThreshold(1, 255, "raw");  
  run("Analyze Particles...", "add");  
  close();
  return roiManager("count");
}

//helper function to center ROIs on cropped cells for various applications
function centerROI(ROI_index, Orig_image_width, Orig_image_height) {
  roiManager("Deselect");
  roiManager("Select", ROI_index);  
  Roi.getBounds(roiX, roiY, roiWidth, roiHeight);
  Roi.move((Orig_image_width - roiWidth)/2, (Orig_image_height - roiHeight)/2);
  roiManager("Update");
  roiManager("Deselect");
  roiString = toString(roiX) + "~~~" + toString(roiY) + "~~~" + toString(roiWidth) + "~~~" + toString(roiHeight);
  return split(roiString, "~~~");
}

//helper function to return ROIs to their original locations after various applications
function resetROI(ROI_index, ROI_X, ROI_Y) {
  roiManager("Deselect");
  roiManager("Select", ROI_index);
  Roi.move(ROI_X, ROI_Y);
  roiManager("Update");
  roiManager("Deselect");
}

//helper function for displaying progress bar
function getBar(p1, p2) {
  n = 20;
  bar1 = "--------------------";
  bar2 = "********************";
  index = round(n*(p1/p2));
  //if (index<1) index = 1;
  //if (index>n-1) index = n-1;
  return substring(bar2, 0, index) + substring(bar1, index, n);
}

//helper function for converting time from seconds to days/hours/minutes/seconds
function timeConvert(t) {
  day = 0; hour = 0; min = 0; sec = 0;
  dayText = " days "; hourText = " hours "; minText = " minutes "; secText = " seconds";

  if (t>=86400) {
    day = floor(t/86400);
    if (day==1) dayText = " day ";
    t = t%86400;
  } if (t>=3600) {
    hour = floor(t/3600);
    if (hour==1) hourText = " hour ";
    t = t%3600;
  } if (t>=60) {
    min = floor(t/60);
    if (min==1) minText = " minute ";
    t = t%60;
  }
  sec = Math.ceil(t);
  if (sec==1) secText = " second ";

  if (day!=0) return toString(day) + dayText + toString(hour) + hourText + toString(min) + minText + toString(sec) + secText;
  else if (hour!=0) return toString(hour) + hourText + toString(min) + minText + toString(sec) + secText;
  else if (min!=0) return toString(min) + minText + toString(sec) + secText;
  else return toString(sec) + secText;
}