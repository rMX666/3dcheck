{$Q-}
unit uCript_1;

interface

uses SysUtils;

type
  TArr16xLongword = array [0..15] of Longword;

procedure MD5Init;
procedure MD5Do(M: TArr16xLongword);
procedure MD5Finalize(var a,b,c,d: Longword);
function ROL(c: Longword; n: byte): Longword;
function MD5InString(s: string): string;
//------------------------------------------------------------------------------
procedure RC4Init(key: shortstring);
function RC4EncByte(ch: byte): byte;
function RC4EncString(key: shortstring; s: string): string;

const
  RC4Start = 0;
  RC4End = 255;

implementation

var md5a, md5b, md5c, md5d: Longword;
  rc4i, rc4j: byte;
  rc4s: array [RC4Start..RC4End] of byte;

procedure MD5Init;
begin
  md5a := $67452301;
  md5b := $efcdab89;
  md5c := $98badcfe;
  md5d := $10325476;
end;

function ROL(c: Longword; n :byte): Longword; assembler;
asm
  mov eax, c
  mov cl, n
  rol eax, cl
end;

procedure MD5Do(M: TArr16xLongword);
var a, b, c, d: Longword;// Временные переменные

  procedure FF(var a, b, c, d, m: Longword; s: byte; t: Longword);
  begin
    a := b + (rol((a + ((b and c)or((not b) and d)) + m + t), s));
  end;

  procedure GG(var a, b, c, d, m: Longword; s: byte; t: Longword);
  begin
    a := b + (rol((a + ((b and d)or(c and (not d))) + m + t), s));
  end;

  procedure HH(var a, b, c, d, m: Longword; s: byte; t: Longword);
  begin
    a := b + (rol((a + (b xor c xor d) + m + t), s));
  end;

  procedure II(var a, b, c, d, m: Longword; s :byte; t: Longword);
  begin
    a := b + (rol((a + (c xor (b or(not d))) + m + t), s));
  end;

begin //MD5Do
  a := md5a;
  b := md5b;
  c := md5c;
  d := md5d;

  //Этап 1
  FF(a,b,c,d, M[0], 7,$D76AA478);
  FF(d,a,b,c, M[1],12,$E8C7B756);
  FF(c,d,a,b, M[2],17,$242070DB);
  FF(b,c,d,a, M[3],22,$C1BDCEEE);

  FF(a,b,c,d, M[4], 7,$F57C0FAF);
  FF(d,a,b,c, M[5],12,$4787C62A);
  FF(c,d,a,b, M[6],17,$A8304613);
  FF(b,c,d,a, M[7],22,$FD469501);

  FF(a,b,c,d, M[8], 7,$698098D8);
  FF(d,a,b,c, M[9],12,$8B44F7AF);
  FF(c,d,a,b,M[10],17,$FFFF5BB1);
  FF(b,c,d,a,M[11],22,$895CD7BE);

  FF(a,b,c,d,M[12], 7,$6B901122);
  FF(d,a,b,c,M[13],12,$FD987193);
  FF(c,d,a,b,M[14],17,$A679438E);
  FF(b,c,d,a,M[15],22,$49B40821);

  //Этап 2
  GG(a,b,c,d, M[1], 5,$F61E2562);
  GG(d,a,b,c, M[6], 9,$C040B340);
  GG(c,d,a,b,M[11],14,$265E5A51);
  GG(b,c,d,a, M[0],20,$E9B6C7AA);

  GG(a,b,c,d, M[5], 5,$D62F105D);
  GG(d,a,b,c,M[10], 9,$02441453);
  GG(c,d,a,b,M[15],14,$D8A1E681);
  GG(b,c,d,a, M[4],20,$E7D3FBC8);

  GG(a,b,c,d, M[9], 5,$21E1CDE6);
  GG(d,a,b,c,M[14], 9,$C33707D6);
  GG(c,d,a,b, M[3],14,$F4D50D87);
  GG(b,c,d,a, M[8],20,$455A14ED);

  GG(a,b,c,d,M[13], 5,$A9E3E905);
  GG(d,a,b,c, M[2], 9,$FCEFA3F8);
  GG(c,d,a,b, M[7],14,$676F02D9);
  GG(b,c,d,a,M[12],20,$8D2A4C8A);

  //Этап 3
  HH(a,b,c,d, M[5], 4,$FFFA3942);
  HH(d,a,b,c, M[8],11,$8771F681);
  HH(c,d,a,b,M[11],16,$6D9D6122);
  HH(b,c,d,a,M[14],23,$FDE5380C);

  HH(a,b,c,d, M[1], 4,$A4BEEA44);
  HH(d,a,b,c, M[4],11,$4BDECFA9);
  HH(c,d,a,b, M[7],16,$F6BB4B60);
  HH(b,c,d,a,M[10],23,$BEBFBC70);

  HH(a,b,c,d,M[13], 4,$289B7EC6);
  HH(d,a,b,c, M[0],11,$EAA127FA);
  HH(c,d,a,b, M[3],16,$D4EF3085);
  HH(b,c,d,a, M[6],23,$04881D05);

  HH(a,b,c,d, M[9], 4,$D9D4D039);
  HH(d,a,b,c,M[12],11,$E6DB99E5);
  HH(c,d,a,b,M[15],16,$1FA27CF8);
  HH(b,c,d,a, M[2],23,$C4AC5665);

  //Этап 4
  II(a,b,c,d, M[0], 6,$F4292244);
  II(d,a,b,c, M[7],10,$432AFF97);
  II(c,d,a,b,M[14],15,$AB9423A7);
  II(b,c,d,a, M[5],21,$FC93A039);

  II(a,b,c,d,M[12], 6,$655B59C3);
  II(d,a,b,c, M[3],10,$8F0CCC92);
  II(c,d,a,b,M[10],15,$FFEFF47D);
  II(b,c,d,a, M[1],21,$85845DD1);

  II(a,b,c,d, M[8], 6,$6FA87E4F);
  II(d,a,b,c,M[15],10,$FE2CE6E0);
  II(c,d,a,b, M[6],15,$A3014314);
  II(b,c,d,a,M[13],21,$4E0811A1);

  II(a,b,c,d, M[4], 6,$F7537E82);
  II(d,a,b,c,M[11],10,$BD3AF235);
  II(c,d,a,b, M[2],15,$2AD7D2BB);
  II(b,c,d,a, M[9],21,$EB86D391);

  // увеличиваем значение переменных
  inc(md5a, a);
  inc(md5b, b);
  inc(md5c, c);
  inc(md5d, d);
end;

procedure MD5Finalize(var a, b, c, d: Longword);
begin
  a := md5a;
  b := md5b;
  c := md5c;
  d := md5d;
end;

function IntToHexFix(a: LongWord): string;
var b, r: string[8];
begin
  b := IntToHex(a,8);
  SetLength(r,8);

  r[7] := b[1];
  r[8] := b[2];

  r[5] := b[3];
  r[6] := b[4];

  r[3] := b[5];
  r[4] := b[6];

  r[1] := b[7];
  r[2] := b[8];

  Result := r;
end;

function MD5InString(s: string): string;
var a, b, c, d: LongWord;
  Arr: TArr16xLongword;
  i, j: integer;
begin
  for i := length(s) + 1 mod 64 to 64 do s := s + #0;
  MD5Init;
  for i := 1 to length(s) div 64 do
    begin
      for j := 0 to 15 do
        Arr[j] := byte(s[j * 4 + 1]) * $FFFFFF + byte(s[j * 4 + 2]) * $FFFF +
          byte(s[j * 4 + 3]) * $FF + byte(s[j * 4 + 4]);
      MD5Do(Arr);
    end;
  MD5Finalize(a, b , c, d);
  result:=IntToHexFix(a) + IntToHexFix(b) + IntToHexFix(c) + IntToHexFix(d);
end;

//------------------------------------------------------------------------------
procedure RC4Init(key: shortstring);
var k: array[RC4Start..RC4End] of byte;
  i, j, t: byte;
begin
  rc4i := RC4Start;
  rc4j := RC4Start;
  for i := RC4Start to RC4End do rc4s[i] := i;
  j := RC4Start;
  t := length(key);
  for i := RC4Start to RC4End do
    begin
      inc(j);
      k[i] := Byte(key[j]);
      if j = t then j := RC4Start;
    end;
  j := RC4Start;
  for i := RC4Start to RC4End do
    begin
      j :=(j + k[i] + rc4s[i]) mod (RC4End - RC4Start + 1);
      t :=rc4s[i];
      rc4s[i] := rc4s[j];
      rc4s[j] := t;
    end;
end;

function RC4EncByte(ch: byte):byte;
var t: byte;
begin
  rc4i := (rc4i + 1) mod (RC4End - RC4Start + 1);
  rc4j := (rc4j + rc4s[rc4i]) mod (RC4End - RC4Start + 1);
  t := rc4s[rc4i];
  rc4s[rc4i] := rc4s[rc4j];
  rc4s[rc4j] := t;
  t := (rc4s[rc4i] + rc4s[rc4j]) mod (RC4End - RC4Start + 1);
  result := rc4s[t] xor ch;
end;

function RC4EncString(Key: ShortString; S: String): String;
var i: integer;
begin
  RC4Init(Key);
  Result := '';
  for i := 1 to Length(S) do
    Result := Result + Chr(RC4EncByte(Ord(S[i])));
end;

end.
