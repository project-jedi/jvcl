<?php
/* vim: set expandtab tabstop=4 shiftwidth=4: */
// +----------------------------------------------------------------------+
// | PHP Version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997-2003 The PHP Group                                |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.0 of the PHP license,       |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Authors: Richard Heyes <richard@php.net>                             |
// |          Tal Peer <tal@php.net>                                      |
// +----------------------------------------------------------------------+


/*
* Example 1:
* $stdin = File::readAll('php://stdin');
*
* Example 2:
* while ($data = File::read('/blaat/bar')) {
*     // Code...
* }
*/

require_once('PEAR.php');

/**
* The default number of bytes for reading
* @const FILE_DEFAULT_READSIZE
*/
define('FILE_DEFAULT_READSIZE', 1024, true);

/**
* Mode to use for reading from files
* @const FILE_MODE_READ
*/
define('FILE_MODE_READ',   'rb', true);

/**
* Mode to use for truncating files, then writing
* @const FILE_MODE_WRITE
*/
define('FILE_MODE_WRITE',  'wb', true);

/**
* Mode to use for appending to files
* @const FILE_MODE_APPEND
*/
define('FILE_MODE_APPEND', 'ab', true);

/**
* Use this when a shared (read) lock is required
* @const FILE_LOCK_SHARED
*/
define('FILE_LOCK_SHARED',    LOCK_SH, true);

/**
* Use this when an exclusive (write) lock is required
* @const FILE_LOCK_EXCLUSIVE
*/
define('FILE_LOCK_EXCLUSIVE', LOCK_EX, true);

/**
* Class for handling files
*
* A class with common functions for writing,
* reading and handling files and directories
*
*
* @author  Richard Heyes <richard@php.net>
* @author  Tal Peer <tal@php.net>
* @access  public
* @version 0.9
* @package File
*/
class File extends PEAR
{
    /**
    * Destructor
    *
    * Unlocks any locked file pointers and closes all filepointers
    * @access private
    */
    function _File()
    {
        $locks = &PEAR::getStaticProperty('File', 'locks');
        $filePointers = &PEAR::getStaticProperty('File', 'filePointers');

        for ($i = 0; $i < count($locks); $i++) {
            flock($this->locks[$i], LOCK_UN);
        }

        if (!empty($filePointers)) {
            foreach ($filePointers as $fname => $value) {
                foreach ($value as $mode => $value) {
                    @fclose($filePointers[$fname][$mode]);
                }
            }
        }
    }

    /**
    * Handles file pointers. If a file pointer needs to be opened,
    * it will be. If it already exists (based on filename and mode)
    * then the existing one will be returned.
    *
    * @access private
    * @param  string  $filename Filename to be used
    * @param  string  $mode     Mode to open the file in
    * @param  mixed   $lock     Type of lock to use
    * @return mixed             PEAR_Error on error and file pointer resource on success
    */
    function &_getFilePointer($filename, $mode, $lock = false)
    {
        $filePointers = &PEAR::getStaticProperty('File', 'filePointers');

        // Need to open first...
        if (!isset($filePointers[$filename][$mode]) OR !is_resource($filePointers[$filename][$mode])) {

            // Check it exists
            if (FILE_MODE_READ == $mode AND !preg_match('/^(http|https|ftp|php):\/\//i', $filename) AND !file_exists($filename)) {
                return PEAR::raiseError('File does not exist: ' . $filename);

            // Writeable?
            } elseif ( (FILE_MODE_WRITE == $mode OR FILE_MODE_APPEND == $mode)
                        AND !file_exists($filename)
                        AND !is_writeable(dirname($filename))) {

                return PEAR::raiseError('Could not create file: ' . $filename);

            } elseif ( (FILE_MODE_WRITE == $mode OR FILE_MODE_APPEND == $mode)
                        AND !is_writeable(dirname($filename))) {

                return PEAR::raiseError('File is not writeable: ' . $filename);
            }

            $filePointers[$filename][$mode] = @fopen($filename, $mode);
            if (false === $filePointers[$filename][$mode]) {
                return PEAR::raiseError('Failed to open file: ' . $filename);
            }
        }

        // Lock it?
        if ($lock) {
            $locks = &PEAR::getStaticProperty('File', 'locks');
            if (flock($filePointers[$filename][$mode], $lock)) {
                $this->locks[] = &$filePointers[$filename][$mode];
            }
        }
        return $filePointers[$filename][$mode];
    }

    /**
    * Reads an entire file and returns it.
    *
    * @access public
    * @param  string $filename Name of file to read from
    * @param  mixed  $lock     Type of lock to use
    * @return mixed            PEAR_Error if an error has occured or a string with the contents of the the file
    */
    function readAll($filename, $lock = false)
    {
        $file = '';
        while (($tmp = File::read($filename, FILE_DEFAULT_READSIZE, $lock)) !== FALSE) {
            if (PEAR::isError($tmp)) {
                return $tmp;
            }
            $file .= $tmp;
        }

        return $file;
    }

    /**
    * Returns a specified number of bytes of a file. Defaults to 1024.
    *
    * @access public
    * @param  string  $filename Name of file to read from
    * @param  integer $size     Bytes to read
    * @param  mixed   $lock     Type of lock to use
    * @return mixed             PEAR_Error on error or a string which contains the data read
    *                           Will also return false upon EOF
    */
    function read($filename, $size = FILE_DEFAULT_READSIZE, $lock = false)
    {
        static $filePointers; // Used to prevent unnecessary calls to _getFilePointer()

        if (0 == $size) {
            return File::readAll($filename);
        }

        if (!isset($filePointers[$filename]) OR !is_resource($filePointers[$filename])) {
            if (PEAR::isError($fp = &File::_getFilePointer($filename, FILE_MODE_READ, $lock))) {
                return $fp;
            }

            $filePointers[$filename] = &$fp;

        } else {
            $fp = &$filePointers[$filename];
        }

        return !feof($fp) ? fread($fp, $size) : false;
    }

    /**
    * Writes the given data to the given filename. Defaults to no lock, append mode.
    *
    * @access public
    * @param  string $filename Name of file to write to
    * @param  string $data     Data to write to file
    * @param  string $mode     Mode to open file in
    * @param  mixed  $lock     Type of lock to use
    * @return mixed             PEAR_Error on error or number of bytes written to file.
    */
    function write($filename, $data, $mode = FILE_MODE_APPEND, $lock = false)
    {
        if (!PEAR::isError($fp = &File::_getFilePointer($filename, $mode, $lock))) {
            if (($bytes = fwrite($fp, $data, strlen($data))) == -1) {
                return PEAR::raiseError(sprintf('fwrite() call failed to write data: "%s" to file: "%s"', $data, $filename));
            } else {
                return $bytes;
            }
        }

        return $fp;
    }

    /**
    * Reads and returns a single character from given filename
    *
    * @access public
    * @param  string $filename Name of file to read from
    * @param  mixed  $lock     Type of lock to use
    * @return mixed            PEAR_Error on error or one character of the specified file
    */
    function readChar($filename, $lock = false)
    {
        return File::read($filename, 1, $lock);
    }

    /**
    * Writes a single character to a file
    *
    * @access public
    * @param  string $filename Name of file to write to
    * @param  string $char     Character to write
    * @param  string $mode     Mode to use when writing
    * @param  mixed  $lock     Type of lock to use
    * @return mixed            PEAR_Error on error, or 1 on success
    */
    function writeChar($filename, $char, $mode = FILE_MODE_APPEND, $lock = false)
    {
        if (!PEAR::isError($fp = &File::_getFilePointer($filename, $mode, $lock))) {
            if (fwrite($fp, $char, 1) == -1) {
                return PEAR::raiseError(sprintf('fwrite() call failed to write data: "%s" to file: "%s"', $data, $filename));
            } else {
                return 1;
            }
        }

        return $fp;
    }

    /**
    * Returns a line of the file
    *
    * @access public
    * @param  string  $filename Name of file to read from
    * @param  boolean $lock     Type of lock to use
    * @return mixed             PEAR_Error on error or a string containing the line read from file
    */
    function readLine($filename, $lock = false)
    {
        static $filePointers; // Used to prevent unnecessary calls to _getFilePointer()

        if (!isset($filePointers[$filename]) OR !is_resource($filePointers[$filename])) {
            if (PEAR::isError($fp = &File::_getFilePointer($filename, FILE_MODE_READ, $lock))) {
                return $fp;
            }

            $filePointers[$filename] = &$fp;

        } else {
            $fp = &$filePointers[$filename];
        }

		if (feof($fp)) {
			return false;
		}

        $fileString = '';
        while (($fileChar = fgetc($fp)) != "\n" AND !feof($fp)) {
            $fileString .= $fileChar;
        }

        return substr($fileString, -1) == "\r" ? substr($fileString, 0, -1) : $fileString;
    }

    /**
    * Writes a single line, appending a LF (by default)
    *
    * @access public
    * @param  string $filename Name of file to write to
    * @param  string $line     Line of data to be written to file
    * @param  string $mode     Write mode, can be either FILE_MODE_WRITE or FILE_MODE_APPEND
    * @param  string $crlf     The CRLF your system is using. UNIX = \n Windows = \r\n Mac = \r
    * @param  mixed  $lock     Type of lock to use
    * @return mixed            PEAR_Error on error or number of bytes written to file (including appended crlf)
    */
    function writeLine($filename, $line, $mode = FILE_MODE_APPEND, $crlf = "\n", $lock = false)
    {
        if(!PEAR::isError($fp = &File::_getFilePointer($filename, $mode, $lock))){
            if (($bytes = fwrite($fp, $line . $crlf)) == -1) {
                return PEAR::raiseError(sprintf('fwrite() call failed to write data: "%s" to file: "%s"', $data, $filename));
            } else {
                return $bytes;
            }
        }

        return $fp;
    }

    /**
    * This rewinds a filepointer to the start of a file
    *
    * @access public
    * @param  string $filename The filename
    * @param  string $mode     Mode the file was opened in
    * @return mixed            PEAR Error on error, true on success
    */
    function rewind($filename, $mode)
    {
        if (!PEAR::isError($fp = &File::_getFilePointer($filename, $mode))) {
            return rewind($fp) ? true : PEAR::raiseError('Failed to rewind file: ' . $filename);
        }

        return $fp;
    }

    /**
    * This closes an open file pointer
    *
    * @access public
    * @param  string $filename The filename that was opened
    * @param  string $mode     Mode the file was opened in
    * @return mixed            PEAR Error on error, true otherwise
    */
    function close($filename, $mode)
    {
        if (!PEAR::isError($fp = &File::_getFilePointer($filename, $mode))) {
            $filePointers = &PEAR::getStaticProperty('File', 'filePointers');
            unset($filePointers[$filename][$mode]);
            return fclose($fp) ? true : PEAR::raiseError('Failed to close file: ' . $filename);
        }

        return $fp;
    }

    /**
    * This unlocks a locked file pointer.
    *
    * @access public
    * @param  string $filename The filename that was opened
    * @param  string $mode     Mode the file was opened in
    * @return mixed            PEAR Error on error, true otherwise
    */
    function unlock($filename, $mode)
    {
        if (!PEAR::isError($fp = &FILE::_getFilePointer($filename, $mode))) {
            return flock($fp, LOCK_UN) ? true : PEAR::raiseError('Failed to unlock file: ' . $filename);
        }

        return $fp;
    }

    /**
    * Returns a string path built from the array $pathParts. Where a join occurs
    * multiple separators are removed. Joins using the optional separator, defaulting
    * to the PHP DIRECTORY_SEPARATOR constant.
    *
    * @access public
    * @param  array  $parts     Array containing the parts to be joined
    * @param  string $separator The system directory seperator
    */
    function buildPath($parts, $separator = DIRECTORY_SEPARATOR)
    {
        for ($i = 0; $i < count($parts); $i++) {
            if (0 == $i) {
                $parts[$i] = File::stripTrailingSeparators($parts[$i], $separator);

            } elseif(count($parts) - 1 == $i) {
                $parts[$i] = File::stripLeadingSeparators($parts[$i], $separator);

            } else {
                $parts[$i] = File::stripTrailingSeparators($parts[$i], $separator);
                $parts[$i] = File::stripLeadingSeparators($parts[$i], $separator);
            }
        }

        return implode($separator, $parts);
    }

    /**
    * Strips trailing separators from the given path
    *
    * @access public
    * @param  string $path      Path to use
    * @param  string $separator Separator to look for
    * @return string            Resulting path
    */
    function stripTrailingSeparators($path, $separator = DIRECTORY_SEPARATOR)
    {
        while (substr($path, -1) == $separator) {
            $path = substr($path, 0, -1);
        }

        return $path;
    }

    /**
    * Strips leading separators from the given path
    *
    * @access public
    * @param  string $path      Path to use
    * @param  string $separator Separator to look for
    * @return string            Resulting path
    */
    function stripLeadingSeparators($path, $separator = DIRECTORY_SEPARATOR)
    {
        while (substr($path, 0, 1) == $separator) {
            $path = substr($path, 1);
        }

        return $path;
    }

    /**
    * Returns a path without leading / or C:\. If this is not
    * present the path is returned as is.
    *
    * @access public
    * @param  string $path The path to be processed
    * @return string       The processed path or the path as is
    */
    function skipRoot($path)
    {
        if (File::isAbsolute($path)) {
            if (DIRECTORY_SEPARATOR == "/") {
                return substr($path,1);

            } elseif(DIRECTORY_SEPARATOR == "\\") {
                return substr($path, 3);
            }
        } else {
            return $path;
        }
    }

    /**
    * Returns the temp directory according to either the TMP, TMPDIR, or TEMP env
    * variables. If these are not set it will also check for the existence of
    * /tmp, %WINDIR%\temp
    *
    * @access public
    * @return string The system tmp directory
    */
    function getTempDir()
    {
        if (OS_WINDOWS){
            if (isset($_ENV['TEMP'])) {
                return $_ENV['TEMP'];
            }
            if (isset($_ENV['TMP'])) {
                return $_ENV['TMP'];
            }
            if (isset($_ENV['windir'])) {
                return $_ENV['windir'] . '\temp';
            }
            return $_ENV['SystemRoot'] . '\temp';
        }
        if (isset($_ENV['TMPDIR'])) {
            return $_ENV['TMPDIR'];
        }
        return '/tmp';
    }

    /**
    * Returns a temporary filename using tempnam() and the above getTmpDir() function.
    *
    * @access public
    * @param  string $dirname Optional directory name for the tmp file
    * @return string          Filename and path of the tmp file
    */
    function getTempFile($dirname = NULL)
    {
		if (is_null($dirname)) {
			$dirname = File::getTempDir();
		}
        return tempnam($dirname, 'temp.');
    }

    /**
    * Returns boolean based on whether given path is absolute or not.
    *
    * @access public
    * @param  string  $path Given path
    * @return boolean       True if the path is absolute, false if it is not
    */
    function isAbsolute($path)
    {
        if (preg_match("/\.\./", $path)) {
            return false;
        }

        if (DIRECTORY_SEPARATOR == '/' AND (substr($path, 0, 1) == '/' OR substr($path, 0, 1) == '~')) {
            return true;

        } elseif(DIRECTORY_SEPARATOR == '\\' AND preg_match('/^[a-z]:\\\/i', $path)) {
            return true;

        }

        return false;
    }
}

PEAR::registerShutdownFunc(array('File', '_File'));

?>
