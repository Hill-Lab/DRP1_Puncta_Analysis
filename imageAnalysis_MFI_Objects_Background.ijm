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

//define measurements for intensity analysis
run("Set Measurements...", "area mean standard min perimeter integrated median area_fraction limit display redirect=None decimal=4");

//enable Bio-Formats Macro Extension commands
run("Bio-Formats Macro Extensions");

//select root folder, check for Original_images folder
//the root folder MUST contain a directory named "Original_images"
rootFolder = getDirectory("Choose a Directory");
if (!File.isDirectory(rootFolder + "Original_images\\")) {
  print("No \"Original_images\" folder exists within " + rootFolder + ".\nTerminating macro execution.");
  exit;
}
originalImagesFolder = rootFolder + "Original_images\\";

//store file list paths, verify they contain images, and automatically detect image file types
originalImagesFileList = getFileList(originalImagesFolder);
originalImagesFileListLength = originalImagesFileList.length;
if (originalImagesFileListLength < 1) {
  print("No images found within " + originalImagesFolder + ".\nTerminating macro execution.");
  exit;
}

//ensure images contain a valid file extension (any additional file types supported by Bio-Formats can be easily added to this list)
imageFileType = split(originalImagesFileList[0], ".");
imageFileType = trim("." + imageFileType[imageFileType.length-1]);
if (imageFileType != ".nd2" && imageFileType != ".lsm" && imageFileType != ".tif" && imageFileType != ".tiff")
  imageFileType = manualFileTypeSelection();

//create directories to save output data analysis files
intensityResults_mean = rootFolder + "intensityResults_mean\\";
File.makeDirectory(intensityResults_mean);
intensityResults_400 = rootFolder + "intensityResults_400\\";
File.makeDirectory(intensityResults_400);
intensityResults_100 = rootFolder + "intensityResults_100\\";
File.makeDirectory(intensityResults_100);

run("ROI Manager...");
roiManager("reset");

//create window for displaying macro progress
progressBar = "[Progress]";
run("Text Window...", "name="+ progressBar +" width=37 height=10 monospaced");
runLog = "[Run Log]";
run("Text Window...", "name="+ runLog +" width=90 height=10");

startTime = getTime();
imagesProcessed = 0;

//iterate through all images
for (i = 0; i < originalImagesFileListLength; i++) {
	print(progressBar, "\\Update:"+ i +"/" + originalImagesFileListLength + " (" + round((i * 100) / originalImagesFileListLength) + "%)\n" + getBar(round(i / originalImagesFileListLength * 100), 100));

	originalImageFileName = originalImagesFileList[i];
  originalImageFileNameNoExt = replace(originalImageFileName, imageFileType, "");
	
	//ensure that the image matches the selected file extension
	if (endsWith(originalImageFileName, imageFileType)) {
		print(runLog, "Opening " + originalImageFileName);

    //open the image
		originalImageFilePath = originalImagesFolder + originalImageFileName;
    run("Bio-Formats Importer", "open=[" + originalImageFilePath + "] color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT use_virtual_stack");
    _original = getImageID();

		//store some basic file info
    getDimensions(origWidth, origHeight, origChannels, origSlices, origFrames);
    getPixelSize(pixelUnit, pixelWidth, pixelHeight);
    run("Set Scale...", "distance=[" + (1/pixelWidth) + "] known=1 unit=[" + pixelUnit + "] global");

    //split images into individual channels, only utilizing the first channel
    print(runLog, "\\Update:Splitting " + originalImageFileName);
    selectImage(_original);
    run("Split Channels");
  	selectImage("C2-" +  originalImageFileName);
  	close();
  	selectImage("C1-" +  originalImageFileName);

    print(runLog, "\\Update:Z-projecting " + originalImageFileName);

    //create a maximum intensity projection of the first channel
  	run("Z Project...", "projection=[Max Intensity]");
  	_maxIP = getImageID();
    selectImage("C1-" +  originalImageFileName);
    close();

    print(runLog, "\\Update:Gathering metrics on " + originalImageFileName);

    //gather metrics on the maximum intensity projection
    selectImage(_maxIP);
    run("Measure");
    setAutoThreshold("Mean dark no-reset");
    run("Measure");
    setAutoThreshold("Mean no-reset");
    run("Measure");

    //save some variables pertaining to data
    objPercentArea1 = getResult("%Area", 0);    
    bgMax1 = getResult("Max", 1);
    bgPercentArea1 = getResult("%Area", 1);

    //count objects present in image (if at least 1% of the image contains fluorescence)
    if ((objPercentArea1 > 1) && (bgPercentArea1 < 99) && (bgMax1 > 16)) {
      setAutoThreshold("Mean dark no-reset");
      run("Analyze Particles...", "size=1-Infinity display summarize");
      selectWindow("Summary");
      saveAs("Results", intensityResults_mean + originalImageFileNameNoExt + "_summary-mean.csv");
      run("Close");
    }

    //save results to corresponding folder
    selectWindow("Results");
    saveAs("Results", intensityResults_mean + originalImageFileNameNoExt + "_results-mean.csv");
    run("Clear Results");

    //set thresholds on the maximum intensity projection and gather metrics
    selectImage(_maxIP);
    run("Measure");
    setThreshold(400, 65535, "raw");
    run("Measure");
    setThreshold(0, 400, "raw");
    run("Measure");

    //save some variables pertaining to data
    objPercentArea2 = getResult("%Area", 0);
    bgMax2 = getResult("Max", 1);
    bgPercentArea2 = getResult("%Area", 1);

    //count objects present in image (if at least 1% of the image contains fluorescence)
    if ((objPercentArea2 > 1) && (bgPercentArea2 < 99) && (bgMax2 > 16)) {
      setThreshold(400, 65535, "raw");
      run("Analyze Particles...", "size=1-Infinity display summarize");
      selectWindow("Summary");
      saveAs("Results", intensityResults_400 + originalImageFileNameNoExt + "_summary-400.csv");
      run("Close");
    }

    //save results to corresponding folder
    selectWindow("Results");
    saveAs("Results", intensityResults_400 + originalImageFileNameNoExt + "_results-400.csv");
    run("Clear Results");

    //set thresholds on the maximum intensity projection and gather metrics
    selectImage(_maxIP);
    run("Measure");
    setThreshold(100, 65535, "raw");
    run("Measure");
    setThreshold(0, 100, "raw");
    run("Measure");

    //save some variables pertaining to data
    objPercentArea3 = getResult("%Area", 0);
    bgMax3 = getResult("Max", 1);
    bgPercentArea3 = getResult("%Area", 1);

    //count objects present in image (if at least 1% of the image contains fluorescence)
    if ((objPercentArea2 > 1) && (bgPercentArea2 < 99) && (bgMax2 > 16)) {
      setThreshold(100, 65535, "raw");
      run("Analyze Particles...", "size=1-Infinity display summarize");
      selectWindow("Summary");
      saveAs("Results", intensityResults_100 + originalImageFileNameNoExt + "_summary-100.csv");
      run("Close");
    }

    //save results to corresponding folder
    selectWindow("Results");
    saveAs("Results", intensityResults_100 + originalImageFileNameNoExt + "_results-100.csv");
    run("Clear Results");
    
    //close images
    selectImage(_maxIP);
    close();   
	}
  print(runLog, "\\Update:Finished processing " + originalImageFileName);
  imagesProcessed++;  
}

//close runtime progress indicator
print(progressBar, "\\Close");
close("ROI Manager");
close("Log");
run("Close All");
endTime = getTime();

//handle some other data to output in log file
outputText = "";
outputText = outputText + "Finished processing a total of " + imagesProcessed + " images\n";
outputText = outputText + "Total time elapsed: " + timeConvert((endTime - startTime)/1000) + "\n";
outputText = outputText + "Average time per image: " + timeConvert(((endTime - startTime)/1000)/imagesProcessed) + "\n";
print(runLog, outputText);
print(runLog, "All ImageJ windows may now be safely closed");


////////////////////////////////////////////////////////////////////////////////////////////////////


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

//helper function for displaying progres bar
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