<?php
/* vim: set expandtab tabstop=4 shiftwidth=4 foldmethod=marker: */
// +----------------------------------------------------------------------+
// | PHP Version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997-2003 The PHP Group                                |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.02 of the PHP license,      |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Author: Tomas V.V.Cox <cox@idecnet.com>                              |
// +----------------------------------------------------------------------+
//
// $Id: ifx.php,v 1.14 2003/08/28 12:20:32 cox Exp $
//
// Database independent query interface definition for PHP's Informix
// extension.
//

// Legend:
// For more info on Informix errors see:
// http://www.informix.com/answers/english/ierrors.htm
//
// TODO:
//  - set needed env Informix vars on connect
//  - implement native prepare/execute

require_once 'DB/common.php';

class DB_ifx extends DB_common
{
    // {{{ properties

    var $connection;
    var $affected = 0;
    var $dsn = array();
    var $transaction_opcount = 0;
    var $autocommit = true;
    var $fetchmode = DB_FETCHMODE_ORDERED; /* Default fetch mode */

    // }}}
    // {{{ constructor

    function DB_ifx()
    {
        $this->phptype = 'ifx';
        $this->dbsyntax = 'ifx';
        $this->features = array(
            'prepare' => false,
            'pconnect' => true,
            'transactions' => true,
            'limit' => 'emulate'
        );
        $this->errorcode_map = array(
            '-201'    => DB_ERROR_SYNTAX,
            '-206'    => DB_ERROR_NOSUCHTABLE,
            '-217'    => DB_ERROR_NOSUCHFIELD,
            '-329'    => DB_ERROR_NODBSELECTED,
            '-1204'   => DB_ERROR_INVALID_DATE,
            '-1205'   => DB_ERROR_INVALID_DATE,
            '-1206'   => DB_ERROR_INVALID_DATE,
            '-1209'   => DB_ERROR_INVALID_DATE,
            '-1210'   => DB_ERROR_INVALID_DATE,
            '-1212'   => DB_ERROR_INVALID_DATE
       );
    }

    // }}}
    // {{{ connect()
    /**
     * Connect to a database and log in as the specified user.
     *
     * @param $dsn the data source name (see DB::parseDSN for syntax)
     * @param $persistent (optional) whether the connection should
     *        be persistent
     *
     * @return int DB_OK on success, a DB error code on failure
     */
    function connect(&$dsninfo, $persistent = false)
    {
        if (!DB::assertExtension('informix') && !DB::assertExtension('Informix')) {
            return $this->raiseError(DB_ERROR_EXTENSION_NOT_FOUND);
        }
        $this->dsn = $dsninfo;
        $dbhost = $dsninfo['hostspec'] ? '@' . $dsninfo['hostspec'] : '';
        $dbname = $dsninfo['database'] ? $dsninfo['database'] . $dbhost : '';
        $user = $dsninfo['username'] ? $dsninfo['username'] : '';
        $pw = $dsninfo['password'] ? $dsninfo['password'] : '';

        $connect_function = $persistent ? 'ifx_pconnect' : 'ifx_connect';

        $this->connection = @$connect_function($dbname, $user, $pw);
        if (!is_resource($this->connection)) {
            return $this->ifxraiseError(DB_ERROR_CONNECT_FAILED);
        }
        return DB_OK;
    }

    // }}}
    // {{{ disconnect()
    /**
     * Log out and disconnect from the database.
     *
     * @return bool TRUE on success, FALSE if not connected.
     */
    function disconnect()
    {
        $ret = @ifx_close($this->connection);
        $this->connection = null;
        return $ret;
    }

    // }}}
    // {{{ simpleQuery()
    /**
     * Send a query to Informix and return the results as a
     * Informix resource identifier.
     *
     * @param $query the SQL query
     *
     * @return int returns a valid Informix result for successful SELECT
     * queries, DB_OK for other successful queries.  A DB error code
     * is returned on failure.
     */
    function simpleQuery($query)
    {
        $ismanip = DB::isManip($query);
        $this->last_query = $query;
        if (preg_match('/(SELECT)/i', $query)) {    //TESTME: Use !DB::isManip()?
            // the scroll is needed for fetching absolute row numbers
            // in a select query result
            $result = @ifx_query($query, $this->connection, IFX_SCROLL);
        } else {
            if (!$this->autocommit && $ismanip) {
                if ($this->transaction_opcount == 0) {
                    $result = @ifx_query('BEGIN WORK', $this->connection);
                    if (!$result) {
                        return $this->ifxraiseError();
                    }
                }
                $this->transaction_opcount++;
            }
            $result = @ifx_query($query, $this->connection);
        }
        if (!$result) {
            return $this->ifxraiseError();
        }
        $this->affected = ifx_affected_rows ($result);
        // Determine which queries should return data, and which
        // should return an error code only.
        if (preg_match('/(SELECT)/i', $query)) {
            return $result;
        }
        // XXX Testme: free results inside a transaction
        // may cause to stop it and commit the work?

        // Result has to be freed even with a insert or update
        ifx_free_result($result);

        return DB_OK;
    }

    // }}}
    // {{{ nextResult()

    /**
     * Move the internal ifx result pointer to the next available result
     *
     * @param a valid fbsql result resource
     *
     * @access public
     *
     * @return true if a result is available otherwise return false
     */
    function nextResult($result)
    {
        return false;
    }

    // }}}
    // {{{ affectedRows()
    /**
     * Gets the number of rows affected by the last query.
     * if the last query was a select, returns an _estimate_ value.
     *
     * @return number of rows affected by the last query
     */
    function affectedRows()
    {
        return $this->affected;
    }

    // }}}
    // {{{ fetchInto()
    /**
     * Fetch a row and return as array.
     *
     * @param   $result     Informix result identifier
     * @param   $row         (reference) array where data from the row is stored
     * @param   $fetchmode      how the resulting array should be indexed
     * @param   $rownum     the row number to fetch
     *
     * @return int an array on success, a DB error code on failure, NULL
     *             if there is no more data
     */
    function fetchInto($result, &$row, $fetchmode, $rownum=null)
    {
        if (($rownum !== null) && ($rownum < 0)) {
            return null;
        }
        // if $rownum is null, fetch row will return the next row
        if (!$row = @ifx_fetch_row($result, $rownum)) {
            return null;
        }
        if ($fetchmode !== DB_FETCHMODE_ASSOC) {
            $i=0;
            $order = array();
            foreach ($row as $key => $val) {
                $order[$i++] = $val;
            }
            $row = $order;
        }
        return DB_OK;
    }

    // }}}
    // {{{ numRows()

    function numRows($result)
    {
        return $this->raiseError(DB_ERROR_NOT_CAPABLE);
    }

    // }}}
    // {{{ numCols()
    /**
     * Get the number of columns in a result set.
     *
     * @param $result Informix result identifier
     *
     * @return int the number of columns per row in $result
     */
    function numCols($result)
    {
        if (!$cols = @ifx_num_fields($result)) {
            return $this->ifxraiseError();
        }
        return $cols;
    }

    // }}}
    // {{{ freeResult()
    /**
     * Free the internal resources associated with $result.
     *
     * @param $result Informix result identifier
     *
     * @return bool TRUE on success, DB_error on error
     */
    function freeResult($result)
    {
        if (is_resource($result)) {
            if (!@ifx_free_result($result)) {
                return $this->ifxraiseError();
            }
            return true;
        }
        if (!isset($this->prepare_tokens[(int)$result])) {
            return false;
        }
        unset($this->prepare_tokens[(int)$result]);
        unset($this->prepare_types[(int)$result]);
        return true;
    }

    // }}}
    // {{{ autoCommit()

    /**
     * Enable/disable automatic commits
     */
    function autoCommit($onoff = true)
    {
        // XXX if $this->transaction_opcount > 0, we should probably
        // issue a warning here.
        $this->autocommit = $onoff ? true : false;
        return DB_OK;
    }

    // }}}
    // {{{ commit()

    /**
     * Commit the current transaction.
     */
    function commit()
    {
        if ($this->transaction_opcount > 0) {
            $result = @ifx_query('COMMIT WORK', $this->connection);
            $this->transaction_opcount = 0;
            if (!$result) {
                return $this->ifxRaiseError();
            }
        }
        return DB_OK;
    }

    // }}}
    // {{{ rollback()

    /**
     * Roll back (undo) the current transaction.
     */
    function rollback()
    {
        if ($this->transaction_opcount > 0) {
            $result = @ifx_query('ROLLBACK WORK', $this->connection);
            $this->transaction_opcount = 0;
            if (!$result) {
                return $this->ifxRaiseError();
            }
        }
        return DB_OK;
    }

    // }}}
    // {{{ ifxraiseError()

    function ifxraiseError($errno = null)
    {
        if ($errno === null) {
            $errno = $this->errorCode(ifx_error());
        }

        return $this->raiseError($errno, null, null, null,
                            $this->errorNative());
    }

    // }}}
    // {{{ errorCode()
    /**
     * Map native error codes to DB's portable ones.  Requires that
     * the DB implementation's constructor fills in the $errorcode_map
     * property.
     *
     * @return int a portable DB error code, or DB_ERROR if this DB
     * implementation has no mapping for the given error code.
     */

    function errorCode($nativecode)
    {
        if (ereg('SQLCODE=(.*)]', $nativecode, $match)) {
            $code = $match[1];
            if (isset($this->errorcode_map[$code])) {
                return $this->errorcode_map[$code];
            }
        }
        return DB_ERROR;
    }

    // }}}
    // {{{ errorNative()
    /**
     * Get the native error message of the last error (if any) that
     * occured on the current connection.
     *
     * @return int native Informix error code
     */
    function errorNative()
    {
        return ifx_error() . ' ' . ifx_errormsg();
    }

    // }}}
    // {{{ getSpecialQuery()

    /**
    * Returns the query needed to get some backend info
    * @param string $type What kind of info you want to retrieve
    * @return string The SQL query string
    */
    function getSpecialQuery($type)
    {
        switch ($type) {
            case 'tables':
            default:
                return null;
        }
        return $sql;
    }

    // }}}
}

?>
