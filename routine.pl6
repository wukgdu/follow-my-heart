.('.'.IO) given sub ($x) {for ($x.dir) -> $y {
    $y.d ?? &?ROUTINE($y) !! $y.Str.say;
    #`(
    # not work
    if ($y.d) {
        # say &?ROUTINE; # not same with sub ($x) {}
        # say &?BLOCK;
        &?ROUTINE($y);
    } else {
        say $y.Str;
    }
    )
}}
#`(
# work, from docs.perl6.org
for '.' {
    .Str.say when !.IO.d;
    .IO.dir()».&?BLOCK when .IO.d # lets recurse a little! 
}
)
