# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{��} ne "\x82\xa0";

use GBK;
print "1..1\n";

# Can't find string terminator '"' anywhere before EOF
# �u�I�[���� '"'���t�@�C���̏I�� EOF �܂łɌ�����Ȃ������v
if ("�Ή��\" eq pack('C6',0x91,0xce,0x89,0x9e,0x95,0x5c)) {
    print qq<ok - 1 "TAIOUHYO"\n>;
}
else {
    print qq<not ok - 1 "TAIOUHYO"\n>;
}

__END__

GBK.pm �̏������ʂ��ȉ��ɂȂ邱�Ƃ����҂��Ă���

if ("�Ή��\\" eq pack('C6',0x91,0xce,0x89,0x9e,0x95,0x5c)) {

Shift-JIS�e�L�X�g�𐳂�������
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm
