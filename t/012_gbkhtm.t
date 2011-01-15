# This file is encoded in GBK.
die "This file is not encoded in GBK.\n" if q{あ} ne "\x82\xa0";

use GBK;
print "1..1\n";

# Bareword found where operator expected
# 「裸の語が演算子があってほしい位置に見つかった」
if ("<img alt=\"対応表\" height=115 width=150>" eq sprintf('<img alt="%s" height=115 width=150>',pack('C6',0x91,0xce,0x89,0x9e,0x95,0x5c))) {
    print qq<ok - 1 "<img alt="TAIOUHYO" height=115 width=150>"\n>;
}
else {
    print qq<not ok - 1 "<img alt="TAIOUHYO" height=115 width=150>"\n>;
}

__END__

GBK.pm の処理結果が以下になることを期待している

if ("<img alt=\"対応表\\" height=115 width=150>" eq sprintf('<img alt="%s" height=115 width=150>',pack('C6',0x91,0xce,0x89,0x9e,0x95,0x5c))) {

Shift-JISテキストを正しく扱う
http://homepage1.nifty.com/nomenclator/perl/shiftjis.htm
