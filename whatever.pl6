sub σ($n) { return 1/(1+e**(-$n)); }
say σ(2);
sub tanh1($n) { return 2*σ(2*$n) - 1; }
say tanh1(2);
sub tanh2($n) { return (e**(2*$n)-1)/(e**(2*$n)+1) }
say tanh2(2);
for (1..100) -> $x {
    if (tanh1($x).fmt("%7f") != tanh2($x).fmt("%7f")) {
        say $x ~ " " ~ tanh1($x) ~ " " ~ tanh2($x);
        last;
    }
}
say 2*False;
say exp(2)-1;
say True.Int;
given 42 {
    when Int { say Int }
    when Numeric { say Num }
}

sub postfix:<!> { [*] 1..$^a }
say 5!;

sub accum ($n is copy) {sub {$n += $^a}}
sub accum2 ($n is copy) {-> $a {$n += $a}}
sub accum3 ($n is copy) {$n += *}
my &a = accum 5;
my $a1 = accum 5;
my &a2 = accum2 5;
my &a3 = accum3 5;
say a(10);
say $a1(10);
say a2(10);
say a3(10);
# say prompt('asdf: ');

class A {
    method haha { "a is here" }
}
class B is A {
    method haha { "b is here" }
}
class C is B {
    method haha { "c is here" }
}
my C $t .= new;
say $t.haha;
say $t.+haha;
# docs.perl6.org <postfix> .+ return a list of all results from parents

my @a1 is default(1);
my @a2[1000001] = 1 xx 1000001;
my $i = 0;
my $t1 = now;
loop ($i = 0; $i < 1000; ++$i) {
    @a1[1000000];
}
my $t2 = now;
say $t2 - $t1;
my $t3 = now;
loop ($i = 0; $i < 1000; ++$i) {
    @a2[1000000];
}
my $t4 = now;
say $t4 - $t3;

say @*ARGS;

with 'foo'.IO {
    ENTER {
        say "enter by order1";
    }
    .spurt: "First line is text, then:\nBinary";
    my $fh will enter { say "enter"; } will leave {say "leave"; .close} = .open;
    $fh.get.say;         # OUTPUT: «First line is text, then:» 
    $fh.encoding: Nil;
    $fh.slurp.say;       # OUTPUT: «Buf[uint8]:0x<42 69 6e 61 72 79>» 
    ENTER {
        say "enter by order2";
    }
}

subset shortStr of Str where 0 < .chars < 10;
sub sayshortStr(shortStr $a) {
    say $a;
}
sayshortStr("123asd");
try {
    CATCH {
        default {
            "here is CATCH: {.Str}".say;
        }
    }
    sayshortStr("123asdfghj");
}
sayshortStr("safe");
