# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..2\n";

my $__FILE__ = __FILE__;

if (length('あいうえお') == 10) {
    print qq{ok - 1 length('あいうえお') == 10 $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 length('あいうえお') == 10 $^X $__FILE__\n};
}

if (GBK::length('あいうえお') == 5) {
    print qq{ok - 2 GBK::length('あいうえお') == 5 $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 GBK::length('あいうえお') == 5 $^X $__FILE__\n};
}

__END__
