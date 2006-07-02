<?php
// $Id: mail.php,v 1.14 2003/08/22 06:57:56 jon Exp $

/**
 * The Log_mail class is a concrete implementation of the Log:: abstract class
 * which sends log messages to a mailbox.
 * The mail is actually sent when you close() the logger, or when the destructor
 * is called (when the script is terminated).
 * 
 * PLEASE NOTE that you must create a Log_mail object using =&, like this :
 *  $logger =& Log::factory("mail", "recipient@example.com", ...)
 * 
 * This is a PEAR requirement for destructors to work properly.
 * See http://pear.php.net/manual/en/class.pear.php
 * 
 * @author  Ronnie Garcia <ronnie@mk2.net>
 * @author  Jon Parise <jon@php.net>
 * @version $Revision: 1.14 $
 * @package Log
 */
class Log_mail extends Log
{
    /** 
     * String holding the recipient's email address.
     * @var string
     */
    var $_recipient = '';

    /** 
     * String holding the sender's email address.
     * @var string
     */
    var $_from = '';

    /** 
     * String holding the email's subject.
     * @var string
     */
    var $_subject = '[Log_mail] Log message';

    /**
     * String holding an optional preamble for the log messages.
     * @var string
     */
    var $_preamble = '';

    /**
     * String holding the mail message body.
     * @var string
     */
    var $_message = '';


    /**
     * Constructs a new Log_mail object.
     * 
     * Here is how you can customize the mail driver with the conf[] hash :
     *   $conf['from']    : the mail's "From" header line,
     *   $conf['subject'] : the mail's "Subject" line.
     * 
     * @param string $name      The filename of the logfile.
     * @param string $ident     The identity string.
     * @param array  $conf      The configuration array.
     * @param int    $maxLevel  Maximum level at which to log.
     * @access public
     */
    function Log_mail($name, $ident = '', $conf = array(),
                      $maxLevel = PEAR_LOG_DEBUG)
    {
        $this->_id = md5(microtime());
        $this->_recipient = $name;
        $this->_ident = $ident;
        $this->_mask = Log::UPTO($maxLevel);

        if (!empty($conf['from'])) {
            $this->_from = $conf['from'];
        } else {
            $this->_from = ini_get('sendmail_from');
        }
        
        if (!empty($conf['subject'])) {
            $this->_subject = $conf['subject'];
        }

        if (!empty($conf['preamble'])) {
            $this->_preamble = $conf['preamble'];
        }
        
        /* register the destructor */
        register_shutdown_function(array(&$this, '_Log_mail'));
    }
    
    /**
     * Destructor. Calls close().
     *
     * @access private
     */
    function _Log_mail()
    {
        $this->close();
    }

    /**
     * Starts a new mail message.
     * This is implicitly called by log(), if necessary.
     * 
     * @access public
     */
    function open()
    {
        if (!$this->_opened) {
            if (!empty($this->_preamble)) {
                $this->_message = $this->_preamble . "\n\n";
            }
            $this->_opened = true;
        }
    }

    /**
     * Closes the message, if it is open, and sends the mail.
     * This is implicitly called by the destructor, if necessary.
     * 
     * @access public
     */
    function close()
    {
        if ($this->_opened) {
            if (!empty($this->_message)) {
                $headers = "From: $this->_from\r\n";
                $headers .= "User-Agent: Log_mail\r\n";
                $headers .= "Content-Transfer-Encoding: 8bit";
                
                if (mail($this->_recipient, $this->_subject, 
                				 $this->_message,
                         $headers, "-f".$this->_from) == false) {
                    error_log("Log_mail: Failure executing mail()", 0);
                    return false;
                }
            }
            $this->_opened = false;
        }

        return true;
    }

    /**
     * Writes $message to the currently open mail message.
     * Calls open(), if necessary.
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

        if (!$this->_opened) {
            $this->open();
        }

        /* Extract the string representation of the message. */
        $message = $this->_extractMessage($message);

        $entry = sprintf("%s %s [%s] %s\n", strftime('%b %d %H:%M:%S'),
                         $this->_ident, Log::priorityToString($priority),
                         $message);

        $this->_message .= $entry;

        $this->_announce(array('priority' => $priority, 'message' => $message));

        return true;
    }
}

?>
