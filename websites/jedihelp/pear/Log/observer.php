<?php
// $Id: observer.php,v 1.7 2003/08/02 23:35:40 jon Exp $
// $Horde: horde/lib/Log/observer.php,v 1.5 2000/06/28 21:36:13 jon Exp $

/**
 * The Log_observer:: class implements the Observer end of a Subject-Observer
 * pattern for watching log activity and taking actions on exceptional events.
 *
 * @author  Chuck Hagenbuch <chuck@horde.org>
 * @version $Revision: 1.7 $
 * @since   Horde 1.3
 * @package Log
 */
class Log_observer
{
    /**
     * Instance-specific unique identification number.
     *
     * @var integer
     * @access private
     */
    var $_id = 0;

    /**
     * The minimum priority level of message that we want to hear about.
     * PEAR_LOG_EMERG is the highest priority, so we will only hear messages
     * with an integer priority value less than or equal to ours.  It defaults
     * to PEAR_LOG_INFO, which listens to everything except PEAR_LOG_DEBUG.
     *
     * @var string
     * @access private
     */
    var $_priority = PEAR_LOG_INFO;

    /**
     * Creates a new basic Log_observer instance.
     *
     * @param integer   $priority   The highest priority at which to receive
     *                              log event notifications.
     *
     * @access public
     */
    function Log_observer($priority = PEAR_LOG_INFO)
    {
        $this->_id = md5(microtime());
        $this->_priority = $priority;
    }

    /**
     * Attempts to return a new concrete Log_observer instance of the requested
     * type.
     *
     * @param string    $type       The type of concreate Log_observer subclass
     *                              to return.
     * @param integer   $priority   The highest priority at which to receive
     *                              log event notifications.
     *
     * @return object               The newly created concrete Log_observer
     *                              instance, or an false on an error.
     */
    function factory($type, $priority = PEAR_LOG_INFO)
    {
        $type = strtolower($type);
        $class = 'Log_observer_' . $type;
        $classfile = 'Log/' . $type . '.php';

        /*
         * Attempt to include our version of the named class, but don't treat
         * a failure as fatal.  The caller may have already included their own
         * version of the named class.
         */
        @include_once $classfile;

        /* If the class exists, return a new instance of it. */
        if (class_exists($class)) {
            return new $class($priority);
        }

        return false;
    }

    /**
     * This is a stub method to make sure that Log_Observer classes do
     * something when they are notified of a message.  The default behavior
     * is to just print the message, which is obviously not desireable in
     * practically any situation - which is why you need to override this
     * method. :)
     *
     * @param array     $event      A hash describing the log event.
     */
    function notify($event)
    {
        print_r($event);
    }
}

?>
