# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{��} ne "\x82\xa0";

use GBK;
print "1..1\n";

# �}�b�`����͂��Ȃ̂Ƀ}�b�`���Ȃ��i�P�j
if ("�^�]�Ƌ�" =~ /�^�]/) {
    print qq<ok - 1 "UNTENMENKYO" =~ /UNTEN/>;
}
else {
    print qq<not ok - 1 "UNTENMENKYO" =~ /UNTEN/>;
}

__END__

Shift-JIS�e�L�X�g�𐳂�������
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm