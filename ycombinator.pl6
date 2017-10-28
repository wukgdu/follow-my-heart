sub fact (&f, $n, $c) {
    if $n == 0 {
        $c
    } else {
        f &f, $n - 1, $n * $c;
    }
}
say fact &fact, 10, 1;
say .(10) given sub (Int $x) { $x < 2 ?? 1 !! $x * &?ROUTINE($x - 1); }
sub Y(&f) {
    sub (&x) {
        x(&x)
    }(sub (&y) { f(sub (**@x) { y(&y)(|@x) }) })
}
sub fac (&f) { sub ($n) { $n < 2 ?? 1 !! $n * f($n - 1) } };
sub fib (&f) { sub ($n) { $n < 2 ?? $n !! f($n - 1) + f($n - 2) } };
sub fib2 (&f) {
    sub ($n, $a, $b) {
        if ($n == 0) {
            $b;
        } else {
            f($n - 1, $a + $b, $a);
        }
    }
};
say Y(&fac)(10);
say map Y($_), ^10 for &fac, &fib;
say (Y(&fib2)($_, 1, 0) for 0..9);

