# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あ(い' =~ /あ\(い/) {
    local $^W = 0;
    if ("$&-$1" eq "あ(い-") {
        print "ok - 1 $^X $__FILE__ ('あ(い' =~ /あ\(い/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('あ(い' =~ /あ\(い/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('あ(い' =~ /あ\(い/).\n";
}

__END__
