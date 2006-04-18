<?php 

// Never forget the trailing slash
$mirror_url_root = "http://obones.free.fr/jvcl_daily/"; 

function GetDisplayFileDate($filename)
{
  if (file_exists($filename))
    return date("Y-m-d H:i:s T", filemtime($filename));
  else
    return " - ";
}

function GetShortFileDate($filename)
{
  if (file_exists($filename))
    return date("Y-m-d", filemtime($filename));
  else
    return " - ";
}

function GetDisplayFileSize($filename)
{
  if (!file_exists($filename))
    return " - ";
  $size = filesize($filename);
  
  if ($size > 1024*1024)
    return round($size / (1024*1024), 2)." M";
  else if ($size > 1024)
    return round($size / 1024, 2)." k";
  else
    return $size;
}

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <meta content="text/html; charset=ISO-8859-1"
 http-equiv="content-type">
  <title>Daily packages</title>
  <link rel="STYLESHEET" type="text/css" href="styles/default.css">
</head>
<body>
<h1>JVCL Daily packages
</h1>
<hr style="width: 100%; height: 2px;">
<br>
Welcome to the daily packages website for the JVCL.<br>
On this page, you will find archive files containing a snapshot of the
JVCL done automatically every night.<br>
They contain a copy of the development repository and as such contain
all the latest bug fixes and improvements provided by the JVCL
developers.<br>
As a result, from time to time, the content of those files may not
compile. We will do everything we can to prevent that but we can't
avoid all errors. As an additional information, you may want to have a
look at the <a href="Build%20status.html">Build Status</a> page. <br>
<br>
<font color="red">Note: If you use one of the archives below, you SHOULD use a daily version of
the JCL as well. You can get it here: <a href="http://jcl.sf.net/daily/">http://jcl.sf.net/daily/</a></font><br>
<br>
The latest version is available for download below<br>
<br>
<table
 style="width: 75%; text-align: left; margin-left: auto; margin-right: auto;"
 cellspacing="2" cellpadding="2">
  <tbody>
    <tr>
      <td style="vertical-align: top; font-weight: bold;">File<br>
      </td>
      <td style="vertical-align: top; font-weight: bold;">Date and time<br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Size</span><br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Description</span><br>
      </td>
    </tr>
    <tr>
      <td style="vertical-align: top; white-space: nowrap;">Latest full: <a href="JVCL3-Latest.zip">zip</a>, <a href="JVCL3-Latest.7z">7z</a>
      (Mirror: <a href=<?php print '"'.$mirror_url_root.'JVCL3-'.GetShortFileDate("JVCL3-Latest.zip").'.zip"'?>>zip</a>,
       <a href=<?php print '"'.$mirror_url_root.'JVCL3-'.GetShortFileDate("JVCL3-Latest.zip").'.7z"'?>>7z</a>)</td>
      <td style="vertical-align: top; white-space: nowrap;"><?php print GetDisplayFileDate("JVCL3-Latest.zip"); ?> 
      </td>
      <td style="vertical-align: top; white-space: nowrap;"><?php print GetDisplayFileSize("JVCL3-Latest.zip");?>, <?php print GetDisplayFileSize("JVCL3-Latest.7z");?> 
      </td>
      <td style="vertical-align: top;">The complete set of files,
including the examples and the installer.<br>
      </td>
    </tr>
    <tr>
      <td style="vertical-align: top; white-space: nowrap;">Latest sources: <a href="JVCL3-Source-Latest.zip">zip</a>, <a href="JVCL3-Source-Latest.7z">7z</a>
      (Mirror: <a href=<?php print '"'.$mirror_url_root.'JVCL3-Source-'.GetShortFileDate("JVCL3-Source-Latest.zip").'.zip"'?>>zip</a>,
       <a href=<?php print '"'.$mirror_url_root.'JVCL3-Source-'.GetShortFileDate("JVCL3-Source-Latest.zip").'.7z"'?>>7z</a>)</td>
      <td style="vertical-align: top; white-space: nowrap;"><?php print GetDisplayFileDate("JVCL3-Source-Latest.zip"); ?> 
      </td>
      <td style="vertical-align: top; white-space: nowrap;"><?php print GetDisplayFileSize("JVCL3-Source-Latest.zip");?>, <?php print GetDisplayFileSize("JVCL3-Source-Latest.7z");?> 
      </td>
      <td style="vertical-align: top;">Only the source files, no
examples and no installer<br>
      </td>
    </tr>
  </tbody>
</table>
<br>
<br>
or you can also grab one of the previous complete or source packages.<br>
<br>
<table
 style="width: 75%; text-align: left; margin-left: auto; margin-right: auto;"
 cellspacing="2" cellpadding="2">
  <tbody>
    <tr>
      <td style="vertical-align: top; font-weight: bold;">File<br>
      </td>
      <td style="vertical-align: top; font-weight: bold;">Date and time<br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Size</span><br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Description</span><br>
      </td>
    </tr>
      <?php
      
      $dh = opendir("./");
      $filenames = array();
      while (($filename = readdir($dh)) !== false)
      {
        if (!is_dir($filename) && 
            (substr($filename, 0, 14) == "JVCL3-Source-2") &&
            (substr($filename, -3) == "zip"))
        {
          $filenames[] = $filename;
        }
      }
        
      rsort($filenames);
      
      foreach($filenames as $filename)
      {
        $filename_7z = str_replace(".zip", ".7z", $filename);
        $filename_full = str_replace("JVCL3-Source-2", "JVCL3-2", $filename);
        $filename_full_7z = str_replace(".zip", ".7z", $filename_full);
        $file_date = substr($filename_full, 6, 10);
        
        echo '<tr>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.$file_date.' full: <a href="'.$filename_full.'">zip</a>, <a href="'.$filename_full_7z.'">7z</a>'."\n";
        echo '  (Mirror: <a href="'.$mirror_url_root.$filename_full.'">zip</a>,'."\n";
        echo '   <a href="'.$mirror_url_root.$filename_full_7z.'">7z</a>)'."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileDate($filename_full)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileSize($filename_full).', '.GetDisplayFileSize($filename_full_7z)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top;">The complete set of files, including the examples and the installer.<br>'."\n";
        echo '  </td>'."\n";
        echo '</tr>'."\n";
        echo '<tr>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.$file_date.' sources: <a href="'.$filename.'">zip</a>, <a href="'.$filename_7z.'">7z</a>'."\n";
        echo '  (Mirror: <a href="'.$mirror_url_root.$filename.'">zip</a>,'."\n";
        echo '   <a href="'.$mirror_url_root.$filename_full_7z.'">7z</a>)'."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileDate($filename)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileSize($filename).', '.GetDisplayFileSize($filename_7z)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top;">Only the source files, no examples and no installer<br>'."\n";
        echo '  </td>'."\n";
        echo '</tr>'."\n";
      }
      ?>
  </tbody>
</table>
<br>
The dates are presented according to the ISO standard (YYYY-MM-DD) and the hours are those of the web server (US Pacific time).<br>
Please note that it may take up to two hours for the mirror to get updated after a file has been published here. It is located in 
France, and also hosts the <a href="http://www.7-zip.org/">7zip</a> files. Those archives contain the same files as the zip 
archives but are about 50% smaller.<br>
<br>
Should you have any problems with those files, please do not hesitate
to contact us on our newsgroup here:<br>
<br>
<a href="news://forums.talkto.net/jedi.vcl">news://forums.talkto.net/jedi.vcl</a><br>
<br>
Thank you for considering the JVCL.<br>
<br>
</body>
</html>
