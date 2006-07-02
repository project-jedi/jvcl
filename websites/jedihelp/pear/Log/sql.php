<?php
// $Id: sql.php,v 1.24 2003/08/22 06:57:56 jon Exp $
// $Horde: horde/lib/Log/sql.php,v 1.12 2000/08/16 20:27:34 chuck Exp $

require_once 'DB.php';

/**
 * The Log_sql class is a concrete implementation of the Log::
 * abstract class which sends messages to an SQL server.  Each entry
 * occupies a separate row in the database.
 *
 * This implementation uses PHP's PEAR database abstraction layer.
 *
 * CREATE TABLE log_table (
 *  id          INT NOT NULL,
 *  logtime     TIMESTAMP NOT NULL,
 *  ident       CHAR(16) NOT NULL,
 *  priority    INT NOT NULL,
 *  message     VARCHAR(200),
 *  PRIMARY KEY (id)
 * );
 *
 * @author  Jon Parise <jon@php.net>
 * @version $Revision: 1.24 $
 * @since   Horde 1.3
 * @package Log 
 */
class Log_sql extends Log {

    /** 
     * Array containing the dsn information. 
     * @var string
     */
    var $_dsn = '';

    /** 
     * Object holding the database handle. 
     * @var object
     */
    var $_db = null;

    /**
     * Flag indicating that we're using an existing database connection.
     * @var boolean
     */
    var $_existingConnection = false;

    /** 
     * String holding the database table to use. 
     * @var string
     */
    var $_table = 'log_table';


    /**
     * Constructs a new sql logging object.
     *
     * @param string $name         The target SQL table.
     * @param string $ident        The identification field.
     * @param array $conf          The connection configuration array.
     * @param int $maxLevel        Maximum level at which to log.
     * @access public     
     */
    function Log_sql($name, $ident = '', $conf = array(),
                     $maxLevel = PEAR_LOG_DEBUG)
    {
        $this->_id = md5(microtime());
        $this->_table = $name;
        $this->_ident = $ident;
        $this->_mask = Log::UPTO($maxLevel);

        /* If an existing database connection was provided, use it. */
        if (isset($conf['db'])) {
            $this->_db = &$conf['db'];
            $this->_existingConnection = true;
            $this->_opened = true;
        } else {
            $this->_dsn = $conf['dsn'];
        }
    }

    /**
     * Opens a connection to the database, if it has not already
     * been opened. This is implicitly called by log(), if necessary.
     *
     * @return boolean   True on success, false on failure.
     * @access public     
     */
    function open()
    {
        if (!$this->_opened) {
            $this->_db = &DB::connect($this->_dsn, true);
            if (DB::isError($this->_db)) {
                return false;
            }
            $this->_opened = true;
        }

        return true;
    }

    /**
     * Closes the connection to the database if it is still open and we were
     * the ones that opened it.  It is the caller's responsible to close an
     * existing connection that was passed to us via $conf['db'].
     *
     * @return boolean   True on success, false on failure.
     * @access public     
     */
    function close()
    {
        if ($this->_opened && !$this->_existingConnection) {
            $this->_opened = false;
            return $this->_db->disconnect();
        }

        return true;
    }

    /**
     * Inserts $message to the currently open database.  Calls open(),
     * if necessary.  Also passes the message along to any Log_observer
     * instances that are observing this Log.
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

        /* Build the SQL query for this log entry insertion. */
        $id = $this->_db->nextId('log_id');
        $q = sprintf('insert into %s (id, logtime, ident, priority, message)' .
                     'values(%d, CURRENT_TIMESTAMP, %s, %d, %s)',
                     $this->_table, $id, $this->_db->quote($this->_ident),
                     $priority, $this->_db->quote($message));

        $result = $this->_db->query($q);
        if (DB::isError($result)) {
            return false;
        }

        $this->_announce(array('priority' => $priority, 'message' => $message));

        return true;
    }
}

?>
