# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..1\n";

my $__FILE__ = __FILE__;

$a = "アソソ";
if ($a =~ s/(ア.{2})//) {
    if ($1 eq "アソソ") {
        print qq{ok - 1 "アソソ" =~ s/(ア.{2})// \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 1 "アソソ" =~ s/(ア.{2})// \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 1 "アソソ" =~ s/(ア.{2})// \$1=($1) $^X $__FILE__\n};
}

__END__
