<?php
ini_set('include_path', realpath(dirname(__FILE__) . "/../..") . ":.:" . ini_get('include_path'));

define('DRIVER_DSN', '');
//define('DRIVER_DSN', 'pgsql://postgres@localhost/test');
//define('DRIVER_DSN', 'mysql://root@localhost/test');
//define('DRIVER_DSN', 'oci8://system:manager@');
//define('DRIVER_DSN', 'ibase(firebird)://SYSDBA:masterkey@//opt/interbase/examples/employee.gdb?dialect=3');
//define('DRIVER_DSN', 'mssql://sa@somehost/pubs');
//define('DRIVER_DSN', 'odbc(db2)://db2inst1:XXXX@SAMPLE');
//define('DRIVER_DSN', 'sqlite://dummy:@localhost/' . getcwd() . DIRECTORY_SEPARATOR . 'test.db' );
?>
