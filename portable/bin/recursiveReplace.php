#!/usr/bin/env php
<?php
if (4 > count($argv)) {
    die ("Please specify a string to find and a string to replace as well as at least one path.\n");
}
$args = $argv;
$me = array_shift($args); /* not needed for anything */
$search = array_shift($args);
$replace = array_shift($args);
echo "Replacing:\n$search\n$replace\n\n";
$paths = array();
while (!empty($args)) {
    $path = trim(array_shift($args));
    if (!empty($path)
	&& file_exists($path)
	&& is_readable($path)
	&& is_writeable($path)) {
	$paths[] = realpath($path);
    } else {
	echo "Bad argument: $path";
    }
}
$paths = array_unique($paths);
$files = array();
$visited = array();
while (!empty($paths)) {
    $path = array_pop($paths);
    $visited[] = $path;
    if (is_file($path)) {
	$files[] = $path;
    } elseif (is_dir($path)) {
	$dir = opendir($path);
	if (false === $dir) {
	    die("Could not open $file\n");
	}
	while (false !== ($subfile = readdir($dir))) {
	  $matches = array();
	  if (preg_match('/.*\.([a-zA-Z0-9]+)$/', $subfile, $matches)) {
	    $extension = $matches[1];
	  } else {
	    $extension = '';
	  }
	  if (
	    $subfile != '.'
	    && $subfile != '..'
	    && $subfile != '.svn'
	    && !in_array($extension, array('bak', 'jpg', 'png'))
            && ('~' != substr($subfile, -1))
	  ) {
	    $subfile = $path . "/" . $subfile;
	    if (!is_link($subfile)) {
	      $subfile = realpath($subfile);
	      if (!in_array($subfile, $paths) && !in_array($subfile, $visited)) {
		$paths[] = $subfile;
	      }
	    }
	  }
	}
    } else {
	echo "$path is neither a file nor a directory\n";
    }
}

foreach ($files as $filename) {
    $backup = $filename . ".bak";
    $file = file_get_contents($filename);
    if (false !== $file) {
	$newFile = str_replace($search, $replace, $file);
	if ($file !== $newFile) {
	    if (touch($backup) && is_writable($backup)) {
		if ($handle = fopen($backup, 'wb')) {
		    if (FALSE !== fwrite($handle, $file)) {
			if (fclose($handle)) {
			    if (is_writable($filename)) {
				if ($handle = fopen($filename, 'wb')) {
				    if (FALSE !== fwrite($handle, $newFile)) {
					fclose($handle);
					echo "Changed $filename\n";
				    } else {
					echo "I tried to write to $filename and failed\n";
				    }
				} else {
				    echo "I tried to get a handle on $filename and failed.\n";
				}
			    }  else {
				echo "I can't write to $filename.\n";
			    }
			} else {
			    echo "I tried to close $backup and failed.\n";
			}
		    } else {
			echo "I tried to write to $backup and failed.\n";
		    }
		} else {
		    echo "I tried to get a handle on $backup and failed.\n";
		}
	    } else {
		echo "I can't write to $backup.\n";
	    }
	} else {
	  #echo "No changes are needed for $filename.\n";
	}
    }
}
?>