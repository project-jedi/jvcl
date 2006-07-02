<?php
// $Id: Log.php,v 1.36 2003/09/02 06:43:14 jon Exp $
// $Horde: horde/lib/Log.php,v 1.15 2000/06/29 23:39:45 jon Exp $

define('PEAR_LOG_EMERG',    0);     /** System is unusable */
define('PEAR_LOG_ALERT',    1);     /** Immediately action */
define('PEAR_LOG_CRIT',     2);     /** Critical conditions */
define('PEAR_LOG_ERR',      3);     /** Error conditions */
define('PEAR_LOG_WARNING',  4);     /** Warning conditions */
define('PEAR_LOG_NOTICE',   5);     /** Normal but significant */
define('PEAR_LOG_INFO',     6);     /** Informational */
define('PEAR_LOG_DEBUG',    7);     /** Debug-level messages */

define('PEAR_LOG_ALL',      bindec('11111111'));  /** All messages */
define('PEAR_LOG_NONE',     bindec('00000000'));  /** No message */

/* Log types for PHP's native error_log() function. */
define('PEAR_LOG_TYPE_SYSTEM',  0); /** Use PHP's system logger */
define('PEAR_LOG_TYPE_MAIL',    1); /** Use PHP's mail() function */
define('PEAR_LOG_TYPE_DEBUG',   2); /** Use PHP's debugging connection */
define('PEAR_LOG_TYPE_FILE',    3); /** Append to a file */

/**
 * The Log:: class implements both an abstraction for various logging
 * mechanisms and the Subject end of a Subject-Observer pattern.
 *
 * @author  Chuck Hagenbuch <chuck@horde.org>
 * @author  Jon Parise <jon@php.net>
 * @version $Revision: 1.36 $
 * @since   Horde 1.3
 * @package Log
 */
class Log
{
    /**
     * Indicates whether or not the log can been opened / connected.
     *
     * @var boolean
     * @access private
     */
    var $_opened = false;

    /**
     * Instance-specific unique identification number.
     *
     * @var integer
     * @access private
     */
    var $_id = 0;

    /**
     * The label that uniquely identifies this set of log messages.
     *
     * @var string
     * @access private
     */
    var $_ident = '';

    /*
     * The bitmask of allowed log levels.
     * @var integer
     * @access private
     */
    var $_mask = PEAR_LOG_ALL;

    /**
     * Holds all Log_observer objects that wish to be notified of new messages.
     *
     * @var array
     * @access private
     */
    var $_listeners = array();


    /**
     * Attempts to return a concrete Log instance of $type.
     *
     * @param string $type      The type of concrete Log subclass to return.
     *                          Attempt to dynamically include the code for
     *                          this subclass. Currently, valid values are
     *                          'console', 'syslog', 'sql', 'file', and 'mcal'.
     *
     * @param string $name      The name of the actually log file, table, or
     *                          other specific store to use. Defaults to an
     *                          empty string, with which the subclass will
     *                          attempt to do something intelligent.
     *
     * @param string $ident     The identity reported to the log system.
     *
     * @param array  $conf      A hash containing any additional configuration
     *                          information that a subclass might need.
     *
     * @param int $maxLevel     Maximum priority level at which to log.
     *
     * @return object Log       The newly created concrete Log instance, or an
     *                          false on an error.
     * @access public
     */
    function &factory($type, $name = '', $ident = '', $conf = array(),
                     $maxLevel = PEAR_LOG_DEBUG)
    {
        $type = strtolower($type);
        $class = 'Log_' . $type;
        $classfile = 'Log/' . $type . '.php';

        /*
         * Attempt to include our version of the named class, but don't treat
         * a failure as fatal.  The caller may have already included their own
         * version of the named class.
         */
        @include_once $classfile;

        /* If the class exists, return a new instance of it. */
        if (class_exists($class)) {
            $object =& new $class($name, $ident, $conf, $maxLevel);
            return $object;
        }

        return false;
    }

    /**
     * Attempts to return a reference to a concrete Log instance of $type, only
     * creating a new instance if no log instance with the same parameters
     * currently exists.
     *
     * You should use this if there are multiple places you might create a
     * logger, you don't want to create multiple loggers, and you don't want to
     * check for the existance of one each time. The singleton pattern does all
     * the checking work for you.
     *
     * <b>You MUST call this method with the $var = &Log::singleton() syntax.
     * Without the ampersand (&) in front of the method name, you will not get
     * a reference, you will get a copy.</b>
     *
     * @param string $type      The type of concrete Log subclass to return.
     *                          Attempt to dynamically include the code for
     *                          this subclass. Currently, valid values are
     *                          'console', 'syslog', 'sql', 'file', and 'mcal'.
     *
     * @param string $name      The name of the actually log file, table, or
     *                          other specific store to use.  Defaults to an
     *                          empty string, with which the subclass will
     *                          attempt to do something intelligent.
     *
     * @param string $ident     The identity reported to the log system.
     *
     * @param array $conf       A hash containing any additional configuration
     *                          information that a subclass might need.
     *
     * @param int $maxLevel     Minimum priority level at which to log.
     *
     * @return object Log       The newly created concrete Log instance, or an
     *                          false on an error.
     * @access public
     */
    function &singleton($type, $name = '', $ident = '', $conf = array(),
                        $maxLevel = PEAR_LOG_DEBUG)
    {
        static $instances;
        if (!isset($instances)) $instances = array();

        $signature = serialize(array($type, $name, $ident, $conf, $maxLevel));
        if (!isset($instances[$signature])) {
            $instances[$signature] = &Log::factory($type, $name, $ident, $conf,
                $maxLevel);
        }

        return $instances[$signature];
    }

    /**
     * Abstract implementation of the open() method.
     */
    function open()
    {
        return false;
    }

    /**
     * Abstract implementation of the close() method.
     */
    function close()
    {
        return false;
    }

    /**
     * Abstract implementation of the log() method.
     */
    function log($message, $priority = PEAR_LOG_INFO)
    {
        return false;
    }

    /**
     * A convenience function for logging a emergency event.  It will log a
     * message at the PEAR_LOG_EMERG log level.
     *
     * @param   mixed   $message    String or object containing the message
     *                              to log.
     *
     * @return  boolean True if the message was successfully logged.
     */
    function emerg($message)
    {
        return $this->log($message, PEAR_LOG_EMERG);
    }

    /**
     * A convenience function for logging an alert event.  It will log a
     * message at the PEAR_LOG_ALERT log level.
     *
     * @param   mixed   $message    String or object containing the message
     *                              to log.
     *
     * @return  boolean True if the message was successfully logged.
     */
    function alert($message)
    {
        return $this->log($message, PEAR_LOG_ALERT);
    }

    /**
     * A convenience function for logging a critical event.  It will log a
     * message at the PEAR_LOG_CRIT log level.
     *
     * @param   mixed   $message    String or object containing the message
     *                              to log.
     *
     * @return  boolean True if the message was successfully logged.
     */
    function crit($message)
    {
        return $this->log($message, PEAR_LOG_CRIT);
    }

    /**
     * A convenience function for logging a error event.  It will log a
     * message at the PEAR_LOG_ERR log level.
     *
     * @param   mixed   $message    String or object containing the message
     *                              to log.
     *
     * @return  boolean True if the message was successfully logged.
     */
    function err($message)
    {
        return $this->log($message, PEAR_LOG_ERR);
    }

    /**
     * A convenience function for logging a warning event.  It will log a
     * message at the PEAR_LOG_WARNING log level.
     *
     * @param   mixed   $message    String or object containing the message
     *                              to log.
     *
     * @return  boolean True if the message was successfully logged.
     */
    function warning($message)
    {
        return $this->log($message, PEAR_LOG_WARNING);
    }

    /**
     * A convenience function for logging a notice event.  It will log a
     * message at the PEAR_LOG_NOTICE log level.
     *
     * @param   mixed   $message    String or object containing the message
     *                              to log.
     *
     * @return  boolean True if the message was successfully logged.
     */
    function notice($message)
    {
        return $this->log($message, PEAR_LOG_NOTICE);
    }

    /**
     * A convenience function for logging a information event.  It will log a
     * message at the PEAR_LOG_INFO log level.
     *
     * @param   mixed   $message    String or object containing the message
     *                              to log.
     *
     * @return  boolean True if the message was successfully logged.
     */
    function info($message)
    {
        return $this->log($message, PEAR_LOG_INFO);
    }

    /**
     * A convenience function for logging a debug event.  It will log a
     * message at the PEAR_LOG_DEBUG log level.
     *
     * @param   mixed   $message    String or object containing the message
     *                              to log.
     *
     * @return  boolean True if the message was successfully logged.
     */
    function debug($message)
    {
        return $this->log($message, PEAR_LOG_DEBUG);
    }

    /**
     * Returns the string representation of the message data.
     *
     * If $message is an object, _extractMessage() will attempt to extract
     * the message text using a known method (such as a PEAR_Error object's
     * getMessage() method).  If a known method, cannot be found, the
     * serialized representation of the object will be returned.
     *
     * If the message data is already a string, it will be returned unchanged.
     *
     * @param  mixed $message   The original message data.  This may be a
     *                          string or any object.
     *
     * @return string           The string representation of the message.
     *
     * @access private
     */
    function _extractMessage($message)
    {
        /*
         * If we've been given an object, attempt to extract the message using
         * a known method.  If we can't find such a method, default to the
         * serialized version of the object.
         */
        if (is_object($message)) {
            if (method_exists($message, 'getmessage')) {
                $message = $message->getMessage();
            } else if (method_exists($message, 'tostring')) {
                $message = $message->toString();
            } else {
                $message = serialize($message);
            }
        }

        /* Otherwise, we assume the message is a string. */
        return $message;
    }

    /**
     * Returns the string representation of a PEAR_LOG_* integer constant.
     *
     * @param int $priority     A PEAR_LOG_* integer constant.
     *
     * @return string           The string representation of $priority.
     */
    function priorityToString($priority)
    {
        $priorities = array(
            PEAR_LOG_EMERG   => 'emergency',
            PEAR_LOG_ALERT   => 'alert',
            PEAR_LOG_CRIT    => 'critical',
            PEAR_LOG_ERR     => 'error',
            PEAR_LOG_WARNING => 'warning',
            PEAR_LOG_NOTICE  => 'notice',
            PEAR_LOG_INFO    => 'info',
            PEAR_LOG_DEBUG   => 'debug'
        );

        return $priorities[$priority];
    }

    /**
     * Calculate the log mask for the given priority.
     *
     * @param integer   $priority   The priority whose mask will be calculated.
     *
     * @return integer  The calculated log mask.
     *
     * @access public
     */
    function MASK($priority)
    {
        return (1 << $priority);
    }

    /**
     * Calculate the log mask for all priorities up to the given priority.
     *
     * @param integer   $priority   The maximum priority covered by this mask.
     *
     * @return integer  The calculated log mask.
     *
     * @access public
     */
    function UPTO($priority)
    {
        return ((1 << ($priority + 1)) - 1);
    }

    /**
     * Set and return the level mask for the current Log instance.
     *
     * @param integer $mask     A bitwise mask of log levels.
     *
     * @return integer          The current level mask.
     *
     * @access public
     */
    function setMask($mask)
    {
        $this->_mask = $mask;

        return $this->_mask;
    }

    /**
     * Returns the current level mask.
     *
     * @return interger         The current level mask.
     *
     * @access public
     */
    function getMask()
    {
        return $this->_mask;
    }

    /**
     * Check if the given priority is included in the current level mask.
     *
     * @param integer   $priority   The priority to check.
     *
     * @return boolean  True if the given priority is included in the current
     *                  log mask.
     *
     * @access private
     */
    function _isMasked($priority)
    {
        return (Log::MASK($priority) & $this->_mask);
    }

    /**
     * Adds a Log_observer instance to the list of observers that are listening
     * for messages emitted by this Log instance.
     *
     * @param object    $observer   The Log_observer instance to attach as a
     *                              listener.
     *
     * @param boolean   True if the observer is successfully attached.
     *
     * @access public
     */
    function attach(&$observer)
    {
        if (!is_a($observer, 'Log_observer')) {
            return false;
        }

        $this->_listeners[$observer->_id] = &$observer;

        return true;
    }

    /**
     * Removes a Log_observer instance from the list of observers.
     *
     * @param object    $observer   The Log_observer instance to detach from
     *                              the list of listeners.
     *
     * @param boolean   True if the observer is successfully detached.
     *
     * @access public
     */
    function detach($observer)
    {
        if (!is_a($observer, 'Log_observer') ||
            !isset($this->_listeners[$observer->_id])) {
            return false;
        }

        unset($this->_listeners[$observer->_id]);

        return true;
    }

    /**
     * Informs each registered observer instance that a new message has been
     * logged.
     *
     * @param array     $event      A hash describing the log event.
     *
     * @access private
     */
    function _announce($event)
    {
        foreach ($this->_listeners as $id => $listener) {
            if ($event['priority'] <= $this->_listeners[$id]->_priority) {
                $this->_listeners[$id]->notify($event);
            }
        }
    }

    /**
     * Indicates whether this is a composite class.
     *
     * @return boolean          True if this is a composite class.
     */
    function isComposite()
    {
        return false;
    }

    /**
     * Sets this Log instance's identification string.
     *
     * @param string    $ident      The new identification string.
     *
     * @access public
     */
    function setIdent($ident)
    {
        $this->_ident = $ident;
    }

    /**
     * Returns the current identification string.
     *
     * @return string   The current Log instance's identification string.
     *
     * @access public
     */
    function getIdent()
    {
        return $this->_ident;
    }
}

?>
