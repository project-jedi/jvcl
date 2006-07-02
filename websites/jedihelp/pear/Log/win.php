<?php
// $Id: win.php,v 1.6 2003/08/22 06:57:56 jon Exp $

/**
 * The Log_win class is a concrete implementation of the Log abstract
 * class that logs messages to a separate browser window.
 *
 * The concept for this log handler is based on part by Craig Davis' article
 * entitled "JavaScript Power PHP Debugging:
 *
 *  http://www.zend.com/zend/tut/tutorial-DebugLib.php
 * 
 * @author  Jon Parise <jon@php.net>
 * @version $Revision: 1.6 $
 * @package Log
 */
class Log_win extends Log
{
    /**
     * The name of the output window.
     * @var string
     * @access private
     */
    var $_name = 'LogWindow';

    /**
     * The title of the output window.
     * @var string
     * @access private
     */
    var $_title = 'Log Output Window';

    /**
     * Mapping of log priorities to colors.
     * @var array
     * @access private
     */
    var $_colors = array(
                        PEAR_LOG_EMERG   => 'red',
                        PEAR_LOG_ALERT   => 'orange',
                        PEAR_LOG_CRIT    => 'yellow',
                        PEAR_LOG_ERR     => 'green',
                        PEAR_LOG_WARNING => 'blue',
                        PEAR_LOG_NOTICE  => 'indigo',
                        PEAR_LOG_INFO    => 'violet',
                        PEAR_LOG_DEBUG   => 'black'
                    );

    /**
     * String buffer that holds line that are pending output.
     * @var array
     * @access private
     */
    var $_buffer = array();

    /**
     * Constructs a new Log_win object.
     * 
     * @param string $name     Ignored.
     * @param string $ident    The identity string.
     * @param array  $conf     The configuration array.
     * @param array  $maxLevel Maximum priority level at which to log.
     * @access public
     */
    function Log_win($name, $ident = '', $conf = array(),
                          $maxLevel = PEAR_LOG_DEBUG)
    {
        $this->_id = md5(microtime());
        $this->_name = $name;
        $this->_ident = $ident;
        $this->_mask = Log::UPTO($maxLevel);

        if (isset($conf['title'])) {
            $this->_title = $conf['title'];
        }
        if (isset($conf['colors']) && is_array($conf['colors'])) {
            $this->_colors = $conf['colors'];
        }

        register_shutdown_function(array(&$this, '_Log_win'));
    }

    /**
     * Destructor
     */
    function _Log_win()
    {
        if ($this->_opened || (count($this->_buffer) > 0)) {
            $this->close();
        }
    }

    /**
     * The first time open() is called, it will open a new browser window and
     * prepare it for output.
     *
     * This is implicitly called by log(), if necessary.
     *
     * @access public
     */
    function open()
    {
        if (!$this->_opened) {
?>
<script language="JavaScript">
win = window.open('', '<?php echo $this->_name; ?>', 'toolbar=no,scrollbars,width=600,height=400');
win.document.writeln('<html>');
win.document.writeln('<head>');
win.document.writeln('<title><?php echo $this->_title; ?></title>');
win.document.writeln('<style type="text/css">');
win.document.writeln('body { font-family: monospace; font-size: 8pt; }');
win.document.writeln('td,th { font-size: 8pt; }');
win.document.writeln('td,th { border-bottom: #999999 solid 1px; }');
win.document.writeln('td,th { border-right: #999999 solid 1px; }');
win.document.writeln('</style>');
win.document.writeln('</head>');
win.document.writeln('<body>');
win.document.writeln('<table border="0" cellpadding="2" cellspacing="0">');
win.document.writeln('<tr><th>Time</th><th>Ident</th><th>Message</th></tr>');
</script>
<?php
            $this->_opened = true;
        }
    }

    /**
     * Closes the output stream if it is open.  If there are still pending
     * lines in the output buffer, the output window will be opened so that
     * the buffer can be drained.
     *
     * @access public
     */
    function close()
    {
        /*
         * If there are still lines waiting to be written, open the output
         * window so that we can drain the buffer.
         */
        if (!$this->_opened && (count($this->_buffer) > 0)) {
            $this->open();
        }

        if ($this->_opened) {
            $this->_writeln('</table>');
            $this->_writeln('</body></html>');
            $this->_opened = false;
        }
    }

    /**
     * Writes a single line of text to the output window.
     *
     * @param string    $line   The line of text to write.
     *
     * @access private
     */
    function _writeln($line)
    {
        /* Add this line to our output buffer. */
        $this->_buffer[] = $line;

        /* Buffer the output until this page's headers have been sent. */
        if (!headers_sent()) {
            return;
        }

        /* If we haven't already opened the output window, do so now. */
        if (!$this->_opened) {
            $this->open();
        }

        /* Drain the buffer to the output window. */
        foreach ($this->_buffer as $line) {
            echo "<script language='JavaScript'>\n";
            echo "win.document.writeln('$line');\n";
            echo "self.focus();\n";
            echo "</script>\n";
        }

        /* Now that the buffer has been drained, clear it. */
        $this->_buffer = array();
    }

    /**
     * Logs $message to the output window.  The message is also passed along
     * to any Log_observer instances that are observing this Log.
     * 
     * @param mixed  $message  String or object containing the message to log.
     * @param string $priority The priority of the message.  Valid
     *                  values are: PEAR_LOG_EMERG, PEAR_LOG_ALERT,
     *                  PEAR_LOG_CRIT, PEAR_LOG_ERR, PEAR_LOG_WARNING,
     *                  PEAR_LOG_NOTICE, PEAR_LOG_INFO, and PEAR_LOG_DEBUG.
     *                  The default is PEAR_LOG_INFO.
     * @return boolean  True on success or false on failure.
     * @access public
     */
    function log($message, $priority = PEAR_LOG_INFO)
    {
        /* Abort early if the priority is above the maximum logging level. */
        if (!$this->_isMasked($priority)) {
            return false;
        }

        /* Extract the string representation of the message. */
        $message = $this->_extractMessage($message);

        list($usec, $sec) = explode(' ', microtime());

        /* Build the output line that contains the log entry row. */
        $line  = '<tr align="left" valign="top">';
        $line .= sprintf('<td>%s.%s</td><td>%s</td>',
                         strftime('%T', $sec), substr($usec, 2, 2),
                         $this->_ident);
        $line .= sprintf('<td style="color: %s" width="100%%">%s</td>',
                         $this->_colors[$priority], nl2br($message));
        $line .= '</tr>';

        $this->_writeln($line);

        $this->_announce(array('priority' => $priority, 'message' => $message));

        return true;
    }
}

?>
