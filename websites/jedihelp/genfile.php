<?php

  require_once "data_access.php";
  require_once "utils.php";
  require_once "security.php";
  require_once "pclzip/pclzip.lib.php";

  define("FILES_PER_ITERATION", 10);
  define("DTX_COOKIE_NAME", "DtxGenerationFile");
  define("OUTPUT_DIR_NAME", "output");
  
  function Format($text)
  {
    return Wrap($text, WrappingColumn, IndentationSpaces);
  }
  
  function GetSeeAlsoString($itemInfos)
  {
    $seeAlsoList = "";
    // build the see also information
    if ($itemInfos["SeeAlsoInternal"] != "")
    {
      $baseclass = trim(substr($itemInfos["Name"], 0, strpos($itemInfos["Name"],".")));
      // start with the internal: the value in the DB is a comma separated list 
      // of IDs, so we get the required infos from the database
      $sa_items = GetListOfItems(trim($itemInfos["SeeAlsoInternal"]));
      if (!is_array($sa_items))
        die($sa_items);
      foreach ($sa_items as $sa_item)
      {
        if ($sa_item == $sa_items[0])
          $seeAlsoList .= RemoveBaseClass(trim($sa_item["Name"]), $baseclass);
        else
          $seeAlsoList .= ", ".RemoveBaseClass(trim($sa_item["Name"]), $baseclass);
      }
    }  
    
      
    // add the undefined and external  
    if ($itemInfos["SeeAlsoUndefined"] != "")
    {
      if ($seeAlsoList!="")
        $seeAlsoList .= ", ";
      $seeAlsoList .= trim($itemInfos["SeeAlsoUndefined"]);
    }
      
    if ($itemInfos["SeeAlsoExternal"] != "")
    {
      if ($seeAlsoList != "")
        $seeAlsoList .= ", ";
      $seeAlsoList .= trim($itemInfos["SeeAlsoExternal"]);
    }
    return $seeAlsoList;
  }

  function GetFileContent($unitId, &$filename)
  {
    // generate the unit's file
    $unitInfos = GetUnitInfos($unitId);
    $items = GetItemsInUnit($unitId);
    if (!is_array($items))
      die($items);

    $filename = str_replace(".pas", ".dtx", $unitInfos["Name"]);   
    
    $unitSummary = $unitInfos["Description"];
    $unitDescription = "";
    if (!(strpos($unitSummary, "\r\n")===0))
    {
      $unitDescription = substr($unitSummary, strpos($unitSummary, "\r\n")+2);
      $unitSummary = substr($unitSummary, 0, strpos($unitSummary, "\r\n"));
    }
    
    $content = "";
    $content .= "##Package: ".$unitInfos["Package"]."\r\n";
    $content .= "##Status: ".$unitInfos["Status"]."\r\n";
    
    $content .= "----------------------------------------------------------------------------------------------------\r\n";
    $content .= "@@".str_replace(".dtx", ".pas", $unitInfos["Name"])."\r\n";
    $content .= "Summary\r\n";
    $content .= Format($unitSummary);
    $content .= "Author\r\n";
    $content .= Format($unitInfos["Author"]);
    if ($unitDescription != "")
    {  
      $content .= "Description\r\n";
      $content .= Format($unitDescription);
    }
    $content .= "\r\n";
    
    // generate for all other items 
    foreach ($items as $item)
    {
      $content .= "----------------------------------------------------------------------------------------------------\r\n";
      $content .= "@@".$item["Name"]."\r\n";
      if ($item["Extras"] != "")
        $content .= $item["Extras"]."\r\n";
        
      if ($item["JVCLInfo"] != "")
      {
        $content .= "JVCLInfo\r\n";
        $content .= Format($item["JVCLInfo"]);
      }  
        
      if ($item["Summary"] != "")
      {
        $content .= "Summary\r\n";
        $content .= Format($item["Summary"]);
      }  
        
      if ($item["Description"] != "")
      {
        $content .= "Description\r\n";
        $content .= Format($item["Description"]);
      }  
        
      if ($item["Parameters"] != "")
      {
        $content .= "Parameters\r\n";
        $content .= Format($item["Parameters"]);
      }  
        
      if ($item["ReturnValue"] != "")
      {
        $content .= "Return value\r\n";
        $content .= Format($item["ReturnValue"]);
      }  
        
      $seeAlsoString = GetSeeAlsoString($item);  
      if ($seeAlsoString != "")
      {
        $content .= "See Also\r\n";
        $content .= Format($seeAlsoString);
      }  
        
      $content .= "\r\n";
    }
    return $content;
  } 
  
  if (realpath(__FILE__) == realpath($_SERVER["SCRIPT_FILENAME"]))
  {
    $tpl = new HTML_Template_IT("./"); 
    $tpl->loadTemplatefile("genfile.tpl.html", true, true);
    
    SetCommonFooter($tpl);
      
    $outDirName = dirname(tempnam("jh_", ""))."/".OUTPUT_DIR_NAME;
    if (array_key_exists("UnitId", $_GET))
    {
      $unitId = $_GET["UnitId"];
      $filename = "";
      
      $content = GetFileContent($unitId, $filename);
      
      header("Content-Type: text/dtx");  // replace by text/plain for debug
      header('Content-Disposition: attachment; filename="'.$filename.'"');
      echo $content;
    }
    elseif (array_key_exists("ProjectId", $_GET) || 
            array_key_exists(DTX_COOKIE_NAME, $_COOKIE))
    {
      $startTimeStr = microtime();  
      if (array_key_exists("ProjectId", $_GET))
      {
        // Step 1: We have a project Id for which to generate, we then
        // create the output directory and get the list of units for the
        // given project. We also store the project Id in the relavant
        // cookie for use at step 3.
        
        if (!file_exists($outDirName))
        {
	        if (!mkdir($outDirName))
	        {
	          $tpl->SetVariable("MESSAGE", "Unable to create output directory: ".$outDirName);
	          $tpl->Show();
	          exit;
	        }
        }
        else
        {
        	// If the directory exists, then empty it. This happens when a generation is cancelled
        	// or goes wrong. We have to cleanup the left overs.
					$dirHandle = opendir($outDirName);
					while(($filename = readdir($dirHandle)) !== false)
					{
				    if(is_file($outDirName."/".$filename))
				    {
				      // unlink file
				      unlink($outDirName."/".$filename);
				    }
					}
					closedir($dirHandle);        	
        }

        setcookie(DTX_COOKIE_NAME."_project", $_GET["ProjectId"]);
      
        $unitsInfos = GetUnitsInProject($_GET["ProjectId"]);
        if (is_string($unitsInfos))
          die($unitsInfos);
        $unitIDs = array();
        foreach($unitsInfos as $unitInfo)
        {
          $unitIDs[] = $unitInfo["Id"];
        }
      }
      else
      {
        // Step 2: We have the cookie that indicates the name of the file keeping
        // track of the list of Ids of units left to generate. We then extract the
        // list from the file.
        
        if (!file_exists($outDirName))
        {
          $tpl->SetVariable("MESSAGE", "Output directory does not exist");
          $tpl->Show();
          exit;
        }
          
        $unitIDs = file($_COOKIE[DTX_COOKIE_NAME]);
        unlink($_COOKIE[DTX_COOKIE_NAME]);
      }
      
      $numFiles = 0;
      while(count($unitIDs) > 0 && $numFiles < FILES_PER_ITERATION)
      {
        $unitID = $unitIDs[0];
        
        // get content
        $filename = "";
        $content = GetFileContent(trim($unitID), $filename);
        
        // write file
        $file = fopen($outDirName."/".$filename, "w");
        fwrite($file, $content);
        fclose($file);
          
        // move to next item
        array_shift($unitIDs);
        $numFiles++;
      }
      $endTimeStr = microtime();
      $duration = MicrotimeDifference($startTimeStr, $endTimeStr);

      // if no more items
      if (count($unitIDs) == 0)
      {
        // No more progress file cookie, so that the only cookie left is the
        // project Id cookie.
        setcookie(DTX_COOKIE_NAME, "", time()-3600);
        
        $msg = "All files generated successfuly generated<br>\n".
               "Please wait for the zip file to be automaticaly generated.";
        $tpl->SetVariable("MESSAGE", $msg);
        
        // force a reload that will go to step 3
        $tpl->setCurrentBlock("auto_refresher");
        $tpl->touchBlock("auto_refresher");
        $tpl->parseCurrentBlock("auto_refresher");       

        $tpl->SetVariable("PROJECT_ID", trim($_COOKIE[DTX_COOKIE_NAME."_project"]));
        $tpl->show();
      }
      else
      {
        // else, set the autorefresher, file content and cookie so that next load
        // of the page will go to step 2 again. This is repeated until there are no
        // more units left
        $timeLeft = ($duration+2) * count($unitIDs) / FILES_PER_ITERATION;
        $msg = 
          "Generated $numFiles files in ".round($duration, 4)." seconds<br>\n".
          count($unitIDs)." files remaining, estimated time left: ".
          
          gmstrftime("%T", $timeLeft);
        
        $tpl->SetVariable("MESSAGE", $msg);
        $tpl->setCurrentBlock("auto_refresher");
        $tpl->touchBlock("auto_refresher");
        $tpl->parseCurrentBlock("auto_refresher");       

        $filename = tempnam("", "jh_");
        setcookie(DTX_COOKIE_NAME, $filename);

        $file = fopen($filename, "w");
        foreach($unitIDs as $unitID)
          fwrite($file, trim($unitID)."\n");
        fclose($file);
        
        $tpl->show();
      }
    }
    elseif (array_key_exists(DTX_COOKIE_NAME."_project", $_COOKIE))
    {
      // Step 3: The last cookie is the project Id cookie, so we know
      // that we can create the zip file from the dtx files.      
      
      // create zip
      $zipname = tempnam("", "jh_");
      $zip = new pclzip($zipname);
      $addResult = $zip->add($outDirName."/", PCLZIP_OPT_REMOVE_ALL_PATH);
      if ($addResult == 0)
      {
        $msg = "Error creating zip file:".$zip->errorInfo(true);
        $tpl->SetVariable("MESSAGE", $msg);
        $tpl->Show();
      }
      else
      {
        $projectInfos = GetProjectInfos($_COOKIE[DTX_COOKIE_NAME."_project"]);
        $filename = $projectInfos["Name"].".zip";
        
        // we have the zip, send it
        header("Content-Type: archive/zip");  // replace by text/plain for debug
        header('Content-Disposition: attachment; filename="'.$filename.'"');

// don't use file_get_contents, it might be too recent        
//        echo file_get_contents($zipname);

        $file = fopen($zipname, "rb");
        while (!feof($file))
        {
          echo fread($file, 1024);
        }
        fclose($file);
      }

      // now that the zip is created, we can remove all files and the directory
      unlink($zipname);
      foreach(glob($outDirName."/*") as $fn) 
      {
        unlink($fn);
      } 
      rmdir($outDirName);

      // no more cookie
      setcookie(DTX_COOKIE_NAME."_project", "", time()-3600);
    }
    else
    {
      header("Location: http://".$_SERVER["HTTP_HOST"].dirname($_SERVER["PHP_SELF"])."/index.php");
      exit;
    }
    
  }
?>