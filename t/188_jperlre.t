# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あA]い' =~ /あ[^]]い/) {
    print "not ok - 1 $^X $__FILE__ ('あA]い' =~ /あ[^]]い/)\n";
}
else {
    print "ok - 1 $^X $__FILE__ ('あA]い' =~ /あ[^]]い/)\n";
}

__END__
