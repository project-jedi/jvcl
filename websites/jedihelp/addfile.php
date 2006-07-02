<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "pclzip/pclzip.lib.php";
  require_once "data_access.php";
  require_once "security.php";
  require_once "security_utils.php";
  require_once "utils.php";
  require_once "page_blocks.php";
  require_once "not_a_power_user.php";
  require_once "not_allowed.php";

  if (!CanUpload())
    die(GetNotAllowedPage("You are not authorized to upload files"));
    
  define("FILE_COOKIE_NAME", "jh_filecookie");
  define("FILES_PER_ITERATION", 10);
  
  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("addfile.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl);
   
  function ProcessItem($lines, &$index, $unitId)
  {
    
    $Name = substr($lines[$index], 2);
    $Summary = "";
    $Author = "";
    $Description = "";
    $ReturnValue = "";
    $SeeAlso = "";
    $Parameters = "";
    $Extras = "";
    $JVCLInfo = "";
    
    $curSectionRef = & $Extras; // be careful with case of $curSectionRef
    
    $WrapCol = WrappingColumn;
    $curSectionContent = '';
    do
    {
      $curLine = rtrim($lines[$index]);
      switch (strtolower($curLine))
      {
        case 'summary':
          $curSectionRef = $curSectionContent;
          $curSectionContent = '';
          $curSectionRef = & $Summary;
          break;
        case 'author':
          $curSectionRef = $curSectionContent;
          $curSectionContent = '';
          $curSectionRef = & $Author;
          break;
        case 'description':
          $curSectionRef = $curSectionContent;
          $curSectionContent = '';
          $curSectionRef = & $Description;
          break;
        case 'return value':
          $curSectionRef = $curSectionContent;
          $curSectionContent = '';
          $curSectionRef = & $ReturnValue;
          break;
        case 'see also':
          $curSectionRef = $curSectionContent;
          $curSectionContent = '';
          $curSectionRef = & $SeeAlso;
          break;
        case 'parameters':
          $curSectionRef = $curSectionContent;
          $curSectionContent = '';
          $curSectionRef = & $Parameters;
          break;
        case 'jvclinfo':
          $curSectionRef = $curSectionContent;
          $curSectionContent = '';
          $curSectionRef = & $JVCLInfo;
          break;
        default:
          if (!HasPrefix($curLine, '----------') && !HasPrefix(trim($curLine),"@@"))
          {
            $WrapCol = strlen(trim($curLine));
            $curSectionContent .= $curLine . "\r\n";
          }
      }
  
      $index++;
    }
    while (($index < count($lines)) and !HasPrefix(trim($lines[$index]), '@@'));
  
    // As we got out, we need to set the value of the section that
    // got pointed to last.
    $curSectionRef = Unwrap($curSectionContent, WrappingColumn, IndentationSpaces);

    // go back one line
    $index--;
    
    $Name        = rtrim($Name);
    $Summary     = rtrim($Summary);
    $Author      = rtrim($Author); 
    $Description = rtrim($Description);
    $ReturnValue = rtrim($ReturnValue);
    $SeeAlso     = rtrim($SeeAlso);
    $Parameters  = rtrim($Parameters);
    $Extras      = rtrim($Extras);
    $JVCLInfo    = rtrim($JVCLInfo);
    
    // We can now insert in the database
    if (substr($Name, -4) == ".pas")
    {
    	$Name = str_replace(".pas", ".dtx", $Name);
    	$existTest = UnitExistsByName($Name);
    	if (is_string($existTest))
    	  return "Error while looking for $Name: $existTest";
    	if ($existTest)
    	{
    	  if ($Summary != "")
      	  $Description = $Summary . "\n" . $Description;
	      $updateResult = ModifyUnit($unitId, $Name, $Description, $Author, null, null, GetLoggedUserId());
	      if ($updateResult == "")
	        return "Unit details updated.<br>";
	      else
	        return "Error while updating unit: ".$updateResult."<br>";
    	}
    	else
    	{
	      return "Error while processing unit details, the unit $Name does not exist !!!!<br>";
    	}
    }      
    else
    {
      $itemId = AddItem($unitId, 
                        $Name, 
                        Unwrap($Summary, $WrapCol, IndentationSpaces), 
                        Unwrap($Description, $WrapCol, IndentationSpaces),
                        Unwrap($ReturnValue, $WrapCol, IndentationSpaces), 
                        Unwrap($SeeAlso, $WrapCol, IndentationSpaces), 
                        Unwrap($Parameters, $WrapCol, IndentationSpaces), 
                        Unwrap($Extras, $WrapCol, IndentationSpaces), 
                        Unwrap($JVCLInfo, $WrapCol, IndentationSpaces), 
                        GetLoggedUserId());
  
      if (is_string($itemId))
        return "Error while adding the item: ".$itemId."<br>";
      else
        return "Added item with key <a href=\"item.php?Id=$itemId\">$itemId</a><br>";
    }
  }
  
  function ProcessDtx($filename, $projectId)
  {
    StartAccessToDB();
    
    $lines = file($filename, "r");
    $msg = "processing ".$filename."<br>";
    
    $unitname = basename($filename);
    
    // Delete the existing unit (if any)
    $msg .= "deleting previous values for $unitname, if any...<br>";
    $deleteResult = DeleteUnitAndItemsByUnitName($unitname);
    
    if ($deleteResult != "")
      $msg .= "Error while removing previous values: ".$deleteResult;
    else
    {
      $unitId = -1;
      $index = 0;
      while ($index < count($lines))
      {
        $curline = rtrim($lines[$index]);
        
        if (HasPrefix($curline, '##Package:'))
        {
          $package = substr($curline, strpos($curline, ':')+2);
          $msg .= "found package: $package<br>";
        }
    
        if (HasPrefix($curline, '##Status:'))
        {
          $status = substr($curline, strpos($curline, ':')+2);
          $msg .= "found status: $status<br>";
        }
    
        if (HasPrefix($curline, '@@')) // found start of an item
        {
          $msg.= "found item: $curline - ";
          if ($unitId == -1)
          {
          	$msg .= "Adding unit <a href=\"unit.php?Name=$unitname\">$unitname</a> - ";
            $unitId = AddUnit($unitname, "", "", $package, $status, $projectId, GetLoggedUserId());
            if (is_string($unitId))
            {
              $msg .= $unitId."<br>";
              $unitId = -1;
            }
          }
            
          if ($unitId != -1)
            $msg .= ProcessItem($lines, $index, $unitId);
        }
        
        $index++;      
      }
    }
    
    // delete the file, we don't need it anymore
    unlink($filename);
    
    EndAccessToDB();
    
    return $msg;
  }
  
  function ProcessFiles($files, $projectId, &$tpl)
  {
    $numFiles = 0;
    $result = "";
    while (count($files)>0 && $numFiles < FILES_PER_ITERATION)
    {
      $file = $files[0];
      
      if (is_string($file))
        $filename = $file;
      else
        $filename = $file["filename"];
      
      $result .= ProcessDtx($filename, $projectId)."<br>";
      array_shift($files);
      $numFiles++;
    }
    return $result;
    
    // if there are files left to process then save their names, set the 
    // cookie and the auto refresher
    if (count($files) > 0)
    {
      $filename = tempnam("", "jh_");
      $fileHandle = fopen($filename, "w");
      foreach($files as $file)
      {
        if (is_string($file))
          fwrite($fileHandle, $file."\n");
        else
          fwrite($fileHandle, $file["filename"]."\n");
      }
      fclose($fileHandle);
      
      setcookie(FILE_COOKIE_NAME, $filename);
      setcookie(FILE_COOKIE_NAME."_project", $projectId);
      $msg = $numFiles." files processed sucessfully, ".
             count($files)." files left.<br>\n".
             "Please wait for automatic progress or click ".
             "<a href='addfile.php'>here</a> ".
             "to continue now.<br>\n".$msg;
             
      $tpl->setCurrentBlock("auto_refresher");
      $tpl->touchBlock("auto_refresher");
      $tpl->parseCurrentBlock("auto_refresher");       
    }
    else
    {
      $msg = $numFiles." files processed successfuly<br>\n".$msg;
      setcookie(FILE_COOKIE_NAME, "", time()-3600);
      setcookie(FILE_COOKIE_NAME."_project", "", time()-3600);
    }
  }
  
  if (array_key_exists(FILE_COOKIE_NAME, $_COOKIE))
    $projectId = $_COOKIE[FILE_COOKIE_NAME."_project"];
  else
    $projectId = $_POST["project"];

  if ($projectId == "")
  {
    $msg = "Project Id was not specified correctly.";
    unlink($_FILES["uploadedfile"]["tmp_name"]);
    $tpl->SetVariable("PROCESS_DURATION", "0");
  }
  elseif ($projectId == "-1")
  {
    $msg = "There are no projects in the system.";
    unlink($_FILES["uploadedfile"]["tmp_name"]);
    $tpl->SetVariable("PROCESS_DURATION", "0");
  }
  elseif (!LoggedUserHasAccessToProject($projectId))
  {
    $msg = "You do not have access to this project. Process aborted";
    unlink($_FILES["uploadedfile"]["tmp_name"]);
    $tpl->SetVariable("PROCESS_DURATION", "0");
  }
  elseif (array_key_exists(FILE_COOKIE_NAME, $_COOKIE) && $_COOKIE[FILE_COOKIE_NAME] != "")
  {
    $startTimeStr = microtime();  
    $files = file($_COOKIE[FILE_COOKIE_NAME]);
    unlink($_COOKIE[FILE_COOKIE_NAME]);
    $msg = ProcessFiles($files, $projectId, $tpl);
    $endTimeStr = microtime();
    $duration = MicrotimeDifference($startTimeStr, $endTimeStr);
    $tpl->SetVariable("PROCESS_DURATION", round($duration, 4));
  }
  elseif ($_FILES["uploadedfile"]["size"] == 0 && $_FILES["uploadedfile"]["tmp_name"] == "")
  {
    $msg = "Please indicate a file. This error may also happen if the file was too big.";
    $tpl->SetVariable("PROCESS_DURATION", "0");
  }
  else
  {
    $tmp = explode('.', $_FILES["uploadedfile"]["name"]);
    $ext = $tmp[count($tmp)-1];
    $startTimeStr = microtime();  
    switch ($ext)
    {
      case "zip":
        if (!CanUploadZip())
          die (GetNotAllowedPage("You are not allowed to upload zip files"));
        $directory = dirname($_FILES["uploadedfile"]["tmp_name"]);
        $zip = new pclzip($_FILES["uploadedfile"]["tmp_name"]);
        $extractResult = $zip->extract(PCLZIP_OPT_PATH, $directory);
        if ($extractResult == 0)
          $msg = "Error extracting zip file";
        else
        {
          $msg = ProcessFiles($extractResult, $projectId, $tpl);
          /*$msg = "";
          foreach($extractResult as $file)
          {
            $msg .= ProcessDtx($file["filename"], $_POST["project"])."<br>";
          }*/
        }
        break;
      case "dtx":
        $filename = dirname($_FILES["uploadedfile"]["tmp_name"]).'/'.basename($_FILES["uploadedfile"]["name"]);
        move_uploaded_file($_FILES["uploadedfile"]["tmp_name"], $filename);
        $msg = ProcessDtx($filename, $projectId);
        break;
      default:
        $msg = "Unsuported extension: ".$ext;
    }
    $endTimeStr = microtime();
    $duration = MicrotimeDifference($startTimeStr, $endTimeStr);
    $tpl->SetVariable("PROCESS_DURATION", round($duration, 4));
  }

  $tpl->setVariable("MESSAGE", $msg);

  $tpl->show();
?>