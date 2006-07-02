<?php
/**
 * An example of Listener usage with HTTP_Request. This downloads and saves 
 * the file displaying the progress bar in the process.
 * 
 * Note two things:
 * 1) The file should be run in console, not in browser;
 * 2) You should turn output buffering OFF for this to work properly.
 * 
 * $Id: download-progress.php,v 1.1 2003/10/27 10:48:49 avb Exp $
 */

require_once 'HTTP/Request.php';
require_once 'HTTP/Request/Listener.php';
require_once 'Console/ProgressBar.php';

PEAR::setErrorHandling(PEAR_ERROR_DIE);

set_time_limit(0);

class HTTP_Request_DownloadListener extends HTTP_Request_Listener
{
   /**
    * Handle for the target file
    * @var int
    */
    var $_fp;

   /**
    * Console_ProgressBar intance used to display the indicator
    * @var object
    */
    var $_bar;

   /**
    * Name of the target file
    * @var string
    */
    var $_target;

   /**
    * Number of bytes received so far
    * @var int
    */
    var $_size = 0;

    function HTTP_Request_DownloadListener()
    {
        $this->HTTP_Request_Listener();
    }

   /**
    * Opens the target file
    * @param string Target file name
    * @throws PEAR_Error
    */
    function setTarget($target)
    {
        $this->_target = $target;
        $this->_fp = @fopen($target, 'wb');
        if (!$this->_fp) {
            PEAR::raiseError("Cannot open '{$target}'");
        }
    }

    function update(&$subject, $event, $data = null)
    {
        switch ($event) {
            case 'sentRequest': 
                $this->_target = basename($subject->_url->path);
                break;

            case 'gotHeaders':
                if (isset($data['content-disposition']) &&
                    preg_match('/filename="([^"]+)"/', $data['content-disposition'], $matches)) {

                    $this->setTarget(basename($matches[1]));
                } else {
                    $this->setTarget($this->_target);
                }
                $this->_bar =& new Console_ProgressBar(
                    '* ' . $this->_target . ' %fraction% KB [%bar%] %percent%', '=>', '-', 
                    79, (isset($data['content-length'])? round($data['content-length'] / 1024): 100)
                );
                $this->_size = 0;
                break;

            case 'tick':
                $this->_size += strlen($data);
                $this->_bar->update(round($this->_size / 1024));
                fwrite($this->_fp, $data);
                break;

            case 'gotBody':
                fclose($this->_fp);
                break;

            default:
                PEAR::raiseError("Unhandled event '{$event}'");
        } // switch
    }
}

// Try using any other package if you like, but choose the bigger ones
// to be able to see the progress bar
$url = 'http://pear.php.net/get/HTML_QuickForm-stable';

$req =& new HTTP_Request($url);

$download =& new HTTP_Request_DownloadListener();
$req->attach($download);
$req->sendRequest(false);
?>
