# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{��} ne "\x82\xa0";

use GBK;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('��A��' =~ /��[^]]��/) {
    print "ok - 1 $^X $__FILE__ ('��A��' =~ /��[^]]��/)\n";
}
else {
    print "not ok - 1 $^X $__FILE__ ('��A��' =~ /��[^]]��/)\n";
}

__END__
