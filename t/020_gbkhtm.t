# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{��} ne "\x82\xa0";

use GBK;
print "1..1\n";

$_ = '';

# Substitution replacement not terminated
# �u�u������̒u�������񂪏I�����Ȃ��v
my $eval = eval { s/�\/��/; };
if ($@) {
    print "not ok - 1 eval { s/HYO/URA/; }\n";
}
else {
    print "ok - 1 eval { s/HYO/URA/; }\n";
}

__END__

Shift-JIS�e�L�X�g�𐳂�������
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm