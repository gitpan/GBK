# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..2\n";

my $__FILE__ = __FILE__;

if (GBK::ord('あ') == 0x82A0) {
    print qq{ok - 1 GBK::ord('あ') == 0x82A0 $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 GBK::ord('あ') == 0x82A0 $^X $__FILE__\n};
}

$_ = 'い';
if (GBK::ord == 0x82A2) {
    print qq{ok - 2 \$_ = 'い'; GBK::ord() == 0x82A2 $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 \$_ = 'い'; GBK::ord() == 0x82A2 $^X $__FILE__\n};
}

__END__
