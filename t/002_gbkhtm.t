# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..1\n";

# エラーにはならないけど文字化けする（１）
if ("暴力" eq pack('C4',0x96,0x5c,0x97,0xcd)) {
    print qq<ok - 1 "BORYOKU"\n>;
}
else {
    print qq<not ok - 1 "BORYOKU"\n>;
}

__END__

GBK.pm の処理結果が以下になることを期待している

if ("暴\力" eq pack('C4',0x96,0x5c,0x97,0xcd)) {

Shift-JISテキストを正しく扱う
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm
