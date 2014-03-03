#!/usr/bin/php
<?php
require_once('databaseMachine.php');

$dbm = databaseMachine::getDatabaseMachine('db2276_brownlog');
$databases = $dbm->getColumn("SHOW DATABASES WHERE `Database` NOT IN ('information_schema', 'mysql')");

$queries = array();
foreach ($databases as $database) {
    if ($database == 'performance_schema') {
        continue;
    }
    $tables = $dbm->getColumn("SHOW TABLES FROM `$database`");
    foreach ($tables as $table) {
        $queries[] = "OPTIMIZE TABLE `$database`.`$table`";
        $queries[] = "ANALYZE TABLE `$database`.`$table`";
    }
}
foreach ($queries as $query) {
    #echo "$query\n";
    $dbm->query($query);
}