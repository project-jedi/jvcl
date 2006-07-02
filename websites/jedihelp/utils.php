<?php

require_once("conf.php");
  
define("WrappingColumn", 100);
define("IndentationSpaces", 2);

function MicrotimeDifference($startTimeStr, $endTimeStr) 
{
  list($usec, $sec) = explode(" ", $startTimeStr);
  $startTime = (float)$usec + (float)$sec; 

  list($usec, $sec) = explode(" ", $endTimeStr);
  $endTime = (float)$usec + (float)$sec;
  
  return $endTime - $startTime;
}

function BinarySearchColumnCompare($array, $element, $columnName) 
{
  // Returns the found $element or 0
  $low = 0;
  $high = count($array) - 1;
  while ($low <= $high) 
  { 
    $mid = round(($low + $high) / 2); 
    $curElem = trim($array[$mid][$columnName]);
    if (strcasecmp($element, $curElem) == 0) 
    {
      return $array[$mid];
    }
    else 
    {
      if (strcasecmp($element, $curElem) < 0) 
      {
        $high = $mid-1;
      }
      else 
      {
        $low = $mid+1;
      }
    }
  }
  return 0;  // $element not found
}


function RemoveBaseClass($itemName, $baseclass)
{
  if ($baseclass == "")
    return $itemName;
  else
    return str_replace($baseclass.".", "", $itemName);
}

// process <LINK target[, text]> markers in the text
function ProcessLinks($line, $baseclass="")
{
  if (!(($linkPos = strpos($line, "<LINK ")) === false))
  {
    $baseclass=trim($baseclass);
    
    $linkParam = substr($line, $linkPos + 6, strpos($line, ">", $linkPos+6)-($linkPos+6));
    if ($linkPos > 0)
      $beforeLink = substr($line, 0, $linkPos-1);
    else
      $beforeLink = "";
    $afterLink = substr($line, $linkPos+6+strlen($linkParam)+1);
    
    if (!(($commaPos = strpos($linkParam, ",")) === false))
    {
      $linkTarget = substr($linkParam, 0, $commaPos);
      $linkText = substr($linkParam, $commaPos+1);
    }
    else
    {
      $linkTarget = $linkParam;
      $linkText = $linkParam;
    }

    $badLink = false;
    $linkTargetInfos = GetItemInfosByName($linkTarget);
    if (is_bool($linkTargetInfos))
    {
      $linkTargetInfos = GetItemInfosByName($baseclass.".".$linkTarget);
      if (is_array($linkTargetInfos))
        $linkTarget = $baseclass.".".$linkTarget;
      else
        $badLink = true;
    }
    
    if (!$badLink)
      $line = $beforeLink." <a href=\"item.php?Name=".trim($linkTarget)."\">".trim($linkText)."</a>".$afterLink;
    else
      $line = $beforeLink." ".trim($linkText).$afterLink;
    
    // Call again to repeat if there are other links
    $line = ProcessLinks($line, $baseclass);
  }
  return $line; 
}

// process <EXTLINK [url]>text</EXTLINK> markers in the text
function ProcessExtLinks($line)
{
  if (!(($linkPos = strpos($line, "<EXTLINK")) === false))
  {
    $url = substr($line, $linkPos + 8, strpos($line, ">", $linkPos+8)-($linkPos+8));
    
    if ($linkPos > 0)
      $beforeLink = substr($line, 0, $linkPos-1);
    else
      $beforeLink = "";
    
    $closePos = strpos($line, "</EXTLINK>");
    $linkText = substr($line, 
                       $linkPos+8+strlen($url)+1, 
                       $closePos-($linkPos+8)-strlen($url)-1);
    
    $afterLink = substr($line, $linkPos+8+$closePos+2);

    if ($url == "")
      $url = $linkText;
    
    $line = $beforeLink." <a href=\"".trim($url)."\">".trim($linkText)."</a>".$afterLink;
    
    // Call again to repeat if there are other links
    $line = ProcessExtLinks($line);
  }
  return $line; 
}

function FormatDescription($description, $baseclass="")
{
  $lines = explode("\r\n", $description);
  $inList = false;
  $inCode = false;
  $inTable = false;
  $tableEnd = false;
  $tableStart = false;
  $tableContent = array();
  $result = "";
  foreach ($lines as $line)
  {
    $line = ProcessLinks($line, $baseclass);
    $line = ProcessExtLinks($line);
    
    if (!(strpos(strtolower($line), "<code>")===false))
      $inCode = true;

    if (!(strpos(strtolower($line), "<table>")===false))
    {
      $inTable = true;
      $tableStart = true;
      $tableContent = "";
    }
    if (!(strpos(strtolower($line), "</table>")===false))
    {
      $inTable = false;
      $tableEnd = true;
      $result .= "\n".GenerateTable($tableContent)."\n";
    }
    
    if (substr(ltrim($line), 0, 2) == "- ")
    {
      $line = substr($line, 3);
      if (!$inList)
        $line = "<ul><li>".$line."</li>\n";
      else
        $line = "<li>".$line."</li>\n";
      $inList = true;
    }
    else
    {
      if ($inList)
        $line = "</ul>\n".$line;
      $inList = false;

      if ($inCode)
        $line = EscapeHtmlMarkers($line)."\n";
      else if (!$inTable)
        $line = $line."<br>\n";
    }
    
    // Can't use str_ireplace, it's PHP5 only
    $line = preg_replace("/\\\\<CODE\\\\>/i", "<CODE><PRE>", $line, 1);  
    $line = preg_replace("/\\\\<\/CODE\\\\>/i", "</PRE></CODE>", $line, 1);
    
    if ($inTable && !$tableStart)
      $tableContent[] = $line;
    else if (!$tableEnd && !$tableStart)
      $result .= UnescapeHtmlMarkers($line);

    if (!(strpos(strtolower($line), "</code>")===false))
      $inCode = false;

    $tableEnd = false;
    $tableStart = false;
  }
  
  return $result;
}

function ProcessExtras($extras, $baseclass)
{
  // if there is a COMBINE tag in the description, then go look in that element
  if (!(($combinePos = strpos($extras, "<COMBINE "))===false))
  {
    $combineName = substr($extras, $combinePos + 9, strpos($extras, ">", $combinePos+9)-($combinePos+9));
    $itemInfos = GetItemInfosByName(trim($combineName));
    if (is_string($itemInfos))
      die($itemInfos);
    elseif (is_bool($itemInfos))
    {
      // try again with the baseclass added, just in case
      $itemInfos = GetItemInfosByName(trim($baseclass).".".trim($combineName));
      if (is_string($itemInfos))
        die($itemInfos);
      elseif (is_bool($itemInfos))
        return "Please refer to ".$itemInfos["Name"]." for details.";  
    }
      
    return "Please refer to <a href=\"item.php?Name=".trim($itemInfos["Name"])."\">".trim($itemInfos["Name"])."</a> for details.";
  }
  else
    return htmlspecialchars($extras);
}

function HasPrefix($string, $prefix) 
{
  $pos = strpos($string, $prefix);
  if ($pos === false)
    return false;
  else
    return $pos == 0;
}

function Unwrap($text, $ColumnBreak, $Indentation)
{
  $lines = explode("\r\n", $text);
  
  $ResultLines = array();

  $prevline = "";
  $inTable = false;
  $inCode = false;
  $I = 0;
  foreach ($lines as $line)
  {
    // first remove all indentation (if necessary) and trim spaces
    // on the right
    if (strncmp($line, '  ', $Indentation) == 0)
      $curLine = substr($line, $Indentation);
    else
      $curLine = ltrim($line);
      
    // verify that we are not getting into or out of a table
    if (!(strpos(strtolower($line), "<table>")===false))
      $inTable = true;
    if (!(strpos(strtolower($line), "</table>")===false))
      $inTable = false;

    // verify that we are not getting into or out of a code section
    if (!(strpos(strtolower($line), "<code>")===false))
      $inCode = true;
    if (!(strpos(strtolower($line), "</code>")===false))
      $inCode = false;
    
    
    $curLine = rtrim($curLine);
    $trimmedCurLine = trim($curLine);

    // then do the unwrapping

    // find out if the line break at the end of the current line
    // must be ignored because it was done by the wordwrapping
    // process, not deliberately by the user.
    if (($I > 0) and
      // The line must not be inside a table definition nor inside a code
      // section   
      (!$inTable) and
      (!$inCode) and
      // The previous line before must be longer than 75%
      // of the ColumnBreak
      ((strlen($prevLine) > 75 * $ColumnBreak / 100) or 
        (($trimmedCurLine !="") and !ctype_alpha($trimmedCurLine{0}))) and
      // The current line must not start with the same 2 characters
      // as the line before. This way, we prevent a list enumeration
      // to end up all on the same line
      ((strlen($curLine) >= 2) and
       (strlen($prevLine) >=2) and 
       (strncasecmp($curLine, $prevLine, 2) <> 0)) and
      // The line before must not end with a point
      ($prevLine{strlen($prevLine)-1} <> '.') and
      // The current line must start with a lower case letter
      (!ctype_alpha($trimmedCurLine{0}) or ctype_lower($trimmedCurLine{0})))
    {
      $ResultLines[count($ResultLines)-1] = $ResultLines[count($ResultLines)-1] . ' ' . $curLine;
    }
    else
      $ResultLines[] = $curLine;

    // store the line that will become the previous on the next iteration
    $prevLine = $curLine;
    $I++;
  }

  // Remove all empty lines at the end of the result
  while ((count($ResultLines) > 0 ) and
    ($ResultLines[count($ResultLines)-1] == ''))
    unset($ResultLines[count($ResultLines)-1]);

  return implode("\r\n", $ResultLines);
}

function Insert(&$array, $index, $content)
{
  $array_count = count($array);
  for($i=$array_count;$i>$index;$i--)
  {
    $array[$i] = $array[$i-1];
  }
  $array[$index] = $content;
}

function Wrap($Text, $ColumnBreak, $Indentation)
{
  $Lines = "";
  $curLine = "";
  $breakIndex = 0;
  $I = 0;

  $Lines = explode("\r\n", $Text);

  $I = 0;
  while ($I < count($Lines))
  {
    $curLine = rtrim($Lines[$I]);

    // find out if the current line needs to be split
    // It needs it if its length is greater than the column
    // break index
    if (strlen($curLine) > $ColumnBreak)
    {
      // then find the column where we cut. It's the first
      // space, tab, or dash starting at the end of the line
      $breakIndex = strrpos(substr($curLine, 0, $ColumnBreak), ' ');
      $breakIndex = max($breakIndex, strrpos(substr($curLine, 0, $ColumnBreak), '\t'));
      $breakIndex = max($breakIndex, strrpos(substr($curLine, $ColumnBreak), '-'));

      // Do the splitting
      $Lines[$I] = substr($curLine, 0, $breakIndex);
      Insert($Lines, $I+1, substr($curLine, $breakIndex+1));
    }

    // apply the indentation
    $Lines[$I] = str_repeat(' ', $Indentation) . $Lines[$I];

    // go to next line
    $I++;
  }

  return implode("\r\n", $Lines) . "\r\n";
}


function UnescapeCharsFromMySql($str)
{
  // remove backslashed quotes so they don't get backslashed again
  $str = str_replace("\\'", "'", $str);
  $str = str_replace("\\\"", "\"", $str);
  $str = str_replace("\\\\", "\\", $str);
  return $str;
} 

function EscapeCharsForMySQL($str)
{
  $str = UnescapeCharsFromMySql($str);
  
  // now backslash backslashes then quotes
  $str = str_replace("\\", "\\\\", $str); 
  $str = str_replace("\"", "\\\"", $str); 
  return $str;
}

function UnescapeHtmlMarkers($str)
{
  $str = str_replace("\<", "&lt;", $str);
  $str = str_replace("\>", "&gt;", $str);
  $str = str_replace("\/", "/",    $str);
  return $str;
}

function EscapeHtmlMarkers($str)
{
  $str = str_replace("<", "\<", $str);
  $str = str_replace(">", "\>", $str);
  return $str;
}

function FormatItemName($name)
{
  $name = trim($name);
  $result = $name;
  if (strpos($name, "@")>0)
  {
    $result = substr($name, 0, strpos($name, "@"));
    $result .= "<br><span style=\"margin-left: 20px;\">";
    $result .= substr($name, strpos($name, "@")+1);
    $result .= "</span>"; 
    $result = str_replace("@", "</span><br><span style=\"margin-left: 20px;\">", $result);
  } 
  
  return $result;
}

// For some items, the summary may be empty as the description only contains
// one line. And sometimes, even the description is empty, so we use the 
// Extra Information. This is most often the case for enumeration elements.
function GetSummaryFromItem($item)
{
  $summary = "";	
  if (array_key_exists("Summary", $item))
    $summary = $item["Summary"];
  if ($summary == "")
  {
    $summary = $item["Description"];
    
    if ($summary == "")
      $summary = $item["Extras"];
    
    // if there is a COMBINE tag in the description, then go look in that element
    if (!(($combinePos = strpos($summary, "<COMBINE "))===false))
    {
      $combineName = substr($summary, $combinePos + 9, strpos($summary, ">", $combinePos+9)-($combinePos+9));
      $itemInfos = GetItemInfosByName(trim($combineName));
      $summary = $itemInfos["Summary"];
    }
    
    // remove anything after the first carriage return
    $crlfPos = strpos($summary, "\r\n");  
    if ($crlfPos === false)
    ;
    else
    {
      $summary = substr($summary, 0, $crlfPos);
      if (substr($summary, $crlfPos+2) != "")
        $summary .= "...";
    }
  }
  return $summary;
}

// Replaces the \r\n combinations with <br>\r\n for display in HTML views
function FormatEndLines($text)
{
  return str_replace("\r\n", "<br>\r\n", $text);
}

function FieldQSort(&$array, $fieldName, $firstElement = null, $lastElement = null)
{            
  if (!is_array($array)) 
    return false;

  if (is_null($firstElement))
    $firstElement = 0;

  if (is_null($lastElement)) 
    $lastElement = count($array) - 1;
  
//  echo $array[$firstElement][$fieldName]."<br>"; 
  if ($array[$firstElement][$fieldName] < $array[$lastElement][$fieldName])  // Compare the field 
  {
    $middleElement = floor(($firstElement + $lastElement) / 2);
    
    $compareElement = $array[$middleElement];

    $fromLeft = $firstElement;
    $fromRight = $lastElement;

    while ($fromLeft <= $fromRight) 
    {
      while ($array[$fromLeft] < $compareElement)
        $fromLeft++;
                
      while ($array[$fromRight] > $compareElement)
        $fromRight--;
    
      if ($fromLeft <= $fromRight)
      {
        $tmp = $array[$fromLeft];
        $array[$fromLeft] = $array[$fromRight];
        $array[$fromRight] = $tmp;
        $fromLeft++;
        $fromRight--;
      }
    }

    FieldQSort($array, $fieldName, $firstElement, $fromRight);
    FieldQSort($array, $fieldName, $fromLeft, $lastElement);        
  }
  return true;
}

function MySQLTimeStampToUnixTimeStamp($mysqlTimeStamp)
{
  $year = substr($mysqlTimeStamp, 0, 4);
  $month = substr($mysqlTimeStamp, 4, 2);
  $day = substr($mysqlTimeStamp, 6, 2);
  $hours = substr($mysqlTimeStamp, 8, 2);
  $minutes = substr($mysqlTimeStamp, 10, 2);
  $seconds = substr($mysqlTimeStamp, 12, 2);
  
  $datestr = "$year/$month/$day $hours:$minutes:$seconds";
  return strtotime($datestr);
}

function ObfuscateEmail($email)
{
  $email = str_replace("@", " [at] ", $email);
  $email = str_replace(".", " [dot] ", $email);
  return $email;
}

function GenerateTable($tableContent)
{
  // let's look for a line starting with at least 2 dashes, that will
  // be our "markup" line, indicating where are the columns
  $i=0;
  while (!HasPrefix(ltrim($tableContent[$i]), "--") && $i < count($tableContent))
    $i++;
    
  $indentLength = strlen($tableContent[$i]) - strlen(ltrim($tableContent[$i]));
  $markupLine = trim($tableContent[$i]);
  $tableContent[$i] = ""; // so that it will not be displayed
  $pos = 0;
  $colValue = 0;
  while (($pos < strlen($markupLine)) && !(($pos = strpos($markupLine, ' ', $pos+1))===false))
  {
    // Add if the previous one is not a space already
    // Else, replace the existing value. We always add the indentation
    // length because the markup line has been trimmed
    $colValue = $pos+$indentLength+1;
    if ($markupLine[$pos-1] != ' ')  
      $columns[] = $colValue;
    else
      $columns[count($columns)-1] = $colValue;
  }
  $columns[] = -1;
    
  // $columns now contains as many colum indexes as there are columns to break
  // so we can now process the lines and create an in memory table, allowing
  // us to do unwraping inside cells
  $table = array();
  $rowIndex = 0;
  foreach ($tableContent as $row)
  {
    if (trim($row) != "")
    {
      $tableRow = array();
      
      $prevColumn = 0;
      $colIndex = 0;
      $tableColAdded = false;
      
      foreach ($columns as $column)
      {
        if ($column == -1)
          $cellContent = UnescapeHtmlMarkers(substr($row, $prevColumn));
        else
          $cellContent = UnescapeHtmlMarkers(substr($row, $prevColumn, $column-$prevColumn));

        if (trim($cellContent) != "")
        {
          // if we have at least IndentationSpaces in front of the content, then
          // we consider it is part of the cell just above and we add the content
          // into that cell
          if (HasPrefix($cellContent, str_repeat(" ", IndentationSpaces)))
            $table[$rowIndex-1][$colIndex] .= $cellContent;
          else
          {
            $tableRow[] = $cellContent;  
            $tableColAdded = true;
          }
  
        }
        
        $prevColumn = $column;
        $colIndex++;
      }
      
      if ($tableColAdded)
      {
        $table[] = $tableRow;
        $rowIndex++;
      }
    }
  }

  // Now that the table is generated, we create a string representation for it  
  $result = "<table>\n";
  $cellTag = "th";
  foreach ($table as $row)
  {
    $result .= "<tr>\n";
    foreach ($row as $cell)
    {
      if ($cell != "")
        $result .= "<".$cellTag.">".$cell."</".$cellTag.">\n";
      else
        $result .= "<".$cellTag.">&nbsp;</".$cellTag.">\n";
    }
    $result .= "</tr>\n";
    if ($cellTag == "th")
      $cellTag = "td";
  }
  $result .= "</table>\n";
  return $result;
}


function GetItemImageUrl($itemName)
{
  global $itemImageBaseUrl;
  
  return $itemImageBaseUrl.strtoupper($itemName).".png";
}

function ItemHasImage($itemName)
{
  $url = GetItemImageUrl($itemName);
  $result = ! (! $f1 = @fopen($url, "rb"));

  if ($result)
    fclose($f1);
    
  return $result;
}

// Following function is from comments page here:
// http://fr2.php.net/imagecreate 
/*********************************************/
/* Fonction: ImageCreateFromBMP */
/* Author: DHKold */
/* Contact: admin@dhkold.com */
/* Date: The 15th of June 2005 */
/* Version: 2.0B */
/*********************************************/

function ImageCreateFromBMP($filename)
{
  // Opening file in binary mode
  if (! $f1 = @fopen($filename,"rb")) 
    return FALSE;
  
  // 1 : Opening FILE headers
  $FILE = unpack("vfile_type/Vfile_size/Vreserved/Vbitmap_offset", fread($f1,14));
  if ($FILE['file_type'] != 19778) 
    return FALSE;
  
  // 2 : Loading BMP headers
  $BMP = unpack('Vheader_size/Vwidth/Vheight/vplanes/vbits_per_pixel'.
                '/Vcompression/Vsize_bitmap/Vhoriz_resolution'.
                '/Vvert_resolution/Vcolors_used/Vcolors_important', 
                fread($f1,40));
  $BMP['colors'] = pow(2,$BMP['bits_per_pixel']);
  if ($BMP['size_bitmap'] == 0) 
    $BMP['size_bitmap'] = $FILE['file_size'] - $FILE['bitmap_offset'];
  $BMP['bytes_per_pixel'] = $BMP['bits_per_pixel']/8;
  $BMP['bytes_per_pixel2'] = ceil($BMP['bytes_per_pixel']);
  $BMP['decal'] = ($BMP['width']*$BMP['bytes_per_pixel']/4);
  $BMP['decal'] -= floor($BMP['width']*$BMP['bytes_per_pixel']/4);
  $BMP['decal'] = 4-(4*$BMP['decal']);
  if ($BMP['decal'] == 4) 
    $BMP['decal'] = 0;
  
  // 3 : Loading colors from palette
  $PALETTE = array();
  if ($BMP['colors'] < 16777216)
  {
    $PALETTE = unpack('V'.$BMP['colors'], fread($f1, $BMP['colors']*4));
  }
  
  // 4 : Image creation
  $IMG = fread($f1,$BMP['size_bitmap']);
  $VIDE = chr(0);
  
  $res = imagecreate($BMP['width'], $BMP['height']);
  $imagePalette = array();
  $P = 0;
  $Y = $BMP['height']-1;
  while ($Y >= 0)
  {
    $X=0;
    while ($X < $BMP['width'])
    {
      if ($BMP['bits_per_pixel'] == 24)
        $COLOR = unpack("V",substr($IMG,$P,3).$VIDE);
      elseif ($BMP['bits_per_pixel'] == 16)
      {
        $COLOR = unpack("n",substr($IMG,$P,2));
        $COLOR[1] = $PALETTE[$COLOR[1]+1];
      }
      elseif ($BMP['bits_per_pixel'] == 8)
      {
        $COLOR = unpack("n",$VIDE.substr($IMG,$P,1));
        $COLOR[1] = $PALETTE[$COLOR[1]+1];
      }
      elseif ($BMP['bits_per_pixel'] == 4)
      {
        $COLOR = unpack("n",$VIDE.substr($IMG,floor($P),1));
        if (($P*2)%2 == 0) $COLOR[1] = ($COLOR[1] >> 4) 
          ; 
        else 
          $COLOR[1] = ($COLOR[1] & 0x0F);
        $COLOR[1] = $PALETTE[$COLOR[1]+1];
      }
      elseif ($BMP['bits_per_pixel'] == 1)
      {
        $COLOR = unpack("n",$VIDE.substr($IMG,floor($P),1));
        if (($P*8)%8 == 0) 
          $COLOR[1] = $COLOR[1] >>7;
        elseif (($P*8)%8 == 1) 
          $COLOR[1] = ($COLOR[1] & 0x40)>>6;
        elseif (($P*8)%8 == 2) 
          $COLOR[1] = ($COLOR[1] & 0x20)>>5;
        elseif (($P*8)%8 == 3) 
          $COLOR[1] = ($COLOR[1] & 0x10)>>4;
        elseif (($P*8)%8 == 4) 
          $COLOR[1] = ($COLOR[1] & 0x8)>>3;
        elseif (($P*8)%8 == 5) 
          $COLOR[1] = ($COLOR[1] & 0x4)>>2;
        elseif (($P*8)%8 == 6) 
          $COLOR[1] = ($COLOR[1] & 0x2)>>1;
        elseif (($P*8)%8 == 7) 
          $COLOR[1] = ($COLOR[1] & 0x1);
        $COLOR[1] = $PALETTE[$COLOR[1]+1];
      }
      else
        return FALSE;
      
      // Create a new palette color if it has never been created before
      if (!array_key_exists($COLOR[1], $imagePalette))
        $imagePalette[$COLOR[1]] = imageColorAllocate($res, 
                                                      ($COLOR[1] >> 16)& 0xFF,  // R
                                                      ($COLOR[1] >> 8) & 0xFF,  // G
                                                      ($COLOR[1] >> 0) &0xFF    // B
                                                     );

      // Use the color now that we are sure it exists    
      $colorToUse = $imagePalette[$COLOR[1]];
      
     // echo $X.','.$Y.':'.$COLOR[1].' '.$colorToUse.'<br>';
      
      imagesetpixel($res, $X, $Y, $colorToUse);
      $X++;
      $P += $BMP['bytes_per_pixel'];
    }
    $Y--;
    $P+=$BMP['decal'];
  }
  
  // closing file
  fclose($f1);
  
  return $res;
} 

?>
