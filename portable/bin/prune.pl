#!/usr/bin/perl
my $dir;

for $dir (@ARGV) {
    prune_dir($dir);
}

exit;

sub prune_dir($) {
    my $dir = shift(@_);
    my @filename;
    my $found_files = 0;
    my $child_files = 0;

    if (-d "$dir") {
	opendir(DIR, $dir) || die "I can't read this directory: $dir $!";
	@filename = readdir(DIR);
	closedir DIR;
	foreach my $filename (sort (@filename))  {
	    # ignore . and .. :
	    if ($filename ne '.' && $filename ne '..') {
		if (-d "$dir/$filename") {
		    if (!-e "$dir/$filename") {
			die "Could not find file: $dir/$filename\n"; # nuts!
		    }
		    if (!-r "$dir/$filename") {
			die "This file exists, but I can't read it: $dir/$filename\n"; # looney toons!
		    }
		    if (!-x "$dir/$filename") {
			die "This directory exists, but I can't read it: $dir/$filename\nPlease verify the ownership and permissions"; # looney toons!
		    } else {
			$found_files++;
			$child_files = prune_dir("$dir/$filename");
			if ($child_files == 0) {
			    $found_files--;
			}
		    }
		} else {
		    $found_files++;
		}
	    }
	}
	if ($found_files == 0) {
	    print "Removing empty dir: $dir\n";
	    rmdir($dir);
	}
	return $found_files;
    } else {
	return 1;
    }

}
