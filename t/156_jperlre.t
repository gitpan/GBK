# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あえう' =~ /(あ[^-い]う)/) {
    if ("$1" eq "あえう") {
        print "ok - 1 $^X $__FILE__ ('あえう' =~ /あ[^-い]う/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('あえう' =~ /あ[^-い]う/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('あえう' =~ /あ[^-い]う/).\n";
}

__END__
