# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あいいいいう' =~ /(あい{1,}いう)/) {
    if ("$1" eq "あいいいいう") {
        print "ok - 1 $^X $__FILE__ ('あいいいいう' =~ /あい{1,}いう/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('あいいいいう' =~ /あい{1,}いう/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('あいいいいう' =~ /あい{1,}いう/).\n";
}

__END__
